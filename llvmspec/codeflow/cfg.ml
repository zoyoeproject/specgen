open Exp

let debug_toggle = true

module IndentLogger = struct
  let indent = ref 0

  let rec mkindent n indent = if n > 0 then
      mkindent (n-1) (indent ^ "  ")
    else indent

  let debug (x:('a, out_channel, unit) format): 'a =
     let str = mkindent !indent "" in
     if debug_toggle then
         Printf.printf ("%s" ^^ x) str
       else (Printf.ifprintf stdout x)

  let scope f =
    debug "{%d\n" !indent;
    assert (!indent < 10);
    indent := !indent + 1;
    let c = f () in
    indent := !indent - 1;
    debug "%d}\n" !indent;
    c
end

let debug : ('a, out_channel, unit) format -> 'a = IndentLogger.debug
let scope = IndentLogger.scope

module type Statement = sig
  module Exp: Exp
  type t
  val mkAssign: bool -> Exp.code -> Exp.t option -> Exp.t list -> t
  val mkMutInd: (string * Exp.t * t) list -> t
  val mkLoop: Exp.t list -> t -> t
  val mkFallThrough: unit -> t
  val mkDangling: unit -> t
  val mkRaise: int -> t
  val mkComment: string -> t
  val bind: Exp.t list -> t -> t -> t
  val emit: Emitter.t -> t -> unit
end

module type Block = sig
  type elt
  type t
  val index: t -> int
  val id: t-> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val next: t -> t list
  val elements: t -> elt list
end

module type Translator = sig
  val translate: 'a -> 'b
end

module Graph (B:Block) = struct
  let dfs entries extend_callback contract_callback stop_callback log_callback =
    let rec step_once ready_stack = begin
      assert (List.length ready_stack < 100);
      (* Invariance: if x∈ready_stack then x∈closure *)
      let hint = (List.fold_left (fun acc c-> acc ^ ", " ^
            match c with | Some c -> B.id c | None -> "<>")
        "[" ready_stack) ^ "]" in
      match ready_stack with
      | [] ->
        log_callback "stop" hint;
        stop_callback ()
      | Some b :: tl -> (* Remove it from the scan list *)
        log_callback "extend" hint;
        let ready_track = if extend_callback b then
          (* Normal case, a new node needs to be tracked *)
          List.fold_left (fun acc x ->
            Some x :: (None :: acc)
          ) tl (B.next b)
        else begin
          (* If we are not going to extend it
           * then we need to remove the contract tag
           *)
          if (List.hd tl == None) then List.tl tl
          else (assert false)
        end
        in step_once ready_track
      | None :: tl ->
        log_callback "contract" @@ hint;
        contract_callback ();
        step_once tl
    end in
    step_once entries
end

module AggregateSet (B: Block) = struct

  type elt = B.t
  let count = ref 1
  let gen_index () = count := !count + 1; !count

  module BlockMap = Map.Make(B)
  module BlockSet = Set.Make(B)
  (*
   * Aggregate is a set of blocks that all blocks in int are
   * connected with each other
   *
   *  |<----------------- top aggregate scope ------------->|
   *
   *  +------ aggregate set -------+
   *  |                            |
   *  |  loop_block -> loop_block -+--> merge_block --------+
   *  |    ↑            ↓          |                        |
   *  |  loop_block <- loop_block -+--> merge_block --------+
   *  |                            |
   *  +----------------------------+
   *)
  type t = {
    index: int;
    blocks: BlockSet.t;
    aggregate_map: ((t ref) BlockMap.t) ref;
  }

  let index b = b.index

  let id b =
    let r = BlockSet.fold (fun b acc -> B.id b ^ " " ^ acc) b.blocks "" in
    "[ bset: " ^ r ^ " ]"

  let make bset map = {index=gen_index(); blocks=bset; aggregate_map=map; }
  let within_closure b t = BlockMap.mem b !(t.aggregate_map)

  let find_aggro b t = BlockMap.find b !(t.aggregate_map)

  let compare x y = (index x) - (index y)
  let equal x y = ((index x) = (index y))
  let next bset =
    (* debug " -- current_bset: %s --" (id bset); *)
    snd @@ BlockSet.fold (fun b acc ->
      let nexts = B.next b in
      List.fold_left (fun (total, s) n ->
        if BlockSet.mem n total then (total, s)
        else begin
          if within_closure n bset then
            (* debug " <next of %s: %s> " (B.id b) (B.id n); *)
            let r = find_aggro n bset in
            (BlockSet.union (!r).blocks total, !r :: s)
          else (total, s)
        end
      ) acc nexts
    ) bset.blocks (bset.blocks,[])

  let elements bset = BlockSet.elements bset.blocks

  (*
   * Based on an underlying block cfg structure,
   * We would like to contract all the loop into one complex node.
   *)
  let iter f a =
    ignore @@ BlockMap.fold (fun k v total ->
      if BlockSet.mem k total then total
      else begin
        (f !v);
        BlockSet.union total (!v).blocks
      end
    ) !(a.aggregate_map) BlockSet.empty

end

(* The Basic Blocks we would like to translate *)
module Make (S:Statement) (BasicBlock: Block with type elt = S.Exp.t)
  = struct
  module Statement = S
  module BlockSet = Set.Make(BasicBlock)
  module BlockMap = Map.Make(BasicBlock)
  module BGraph = Graph(BasicBlock)
  module BlockClosure = AggregateSet(BasicBlock)
  module BlockClosureGraph = Graph(BlockClosure)
  module AggroSet = Set.Make(BlockClosure)
  module AggroMap = Map.Make(BlockClosure)

  module Exit = struct
    type t = (Statement.Exp.t * BasicBlock.t)
    let compare (_, b) (_, b') = BasicBlock.compare b b'
  end

  module ExitSet = Set.Make (Exit)

  type entry = string * Statement.Exp.t * BasicBlock.t

  type translator = BasicBlock.t -> ((Statement.Exp.t * Statement.t) * entry list)

  type error =
  | MultiEntry of BlockSet.t

  exception CFGError of (error * BlockClosure.t option)

  let split_loop_exits (b:BasicBlock.t) (es: entry list) =
    List.partition (fun (_, _, b') -> BasicBlock.compare b b' = 0) es

  let debug_aggro aggro =
    BlockClosure.iter (fun aggo ->
      let aggs = BlockClosure.next aggo in
      let nexts = List.fold_left (fun acc a ->
        BlockClosure.id a ^ ";" ^ acc
      ) "" aggs in
      debug "%s => %s\n" (BlockClosure.id aggo) nexts
    ) aggro

  let emitter _ = raise @@ CFGError (MultiEntry BlockSet.empty, None)

  (*
   * Given an entry of type B.t compute entry' of type t.
   * So that entry' is a aggregate of entry and
   * there is no loop starting from entry' in Graph(t)
   *)
  let aggregate entry blockset entry_as_exit: BlockClosure.t =
    debug "aggregating ...\n";
    let aggregate_map: ((BlockSet.t ref) BlockMap.t) ref = ref BlockMap.empty in
    let aggro_map = ref BlockMap.empty in
    let path = ref [] in

    (* get the tail list which starts with x = c *)
    let rec get_tl_for c ls = match ls with
      | [] -> []
      | h::_ when BasicBlock.equal c h -> ls
      | _::tl -> get_tl_for c tl
    in

    (*
     * Suppose that path = x_1, x_2, ... x_k,
     * then, we check whether there is alreay c inside the path.
     * If this is the case, then we mark
     *   path = x_1, x_2,  ... c
     * to be the tl_for path and aggregate them together
     * Otherwise, we add c at the begin of the path
     *   path = c, x_1, x_2, ..., x_k
     *)
    let extend_callback c =
      if ((entry_as_exit && BasicBlock.equal c entry
            && (List.length !path > 0))
         || (not (BlockSet.mem c blockset))) then
        false
      else begin (* A valid extention point *)
        let tl_for_c = get_tl_for c (List.rev !path) in
        match tl_for_c with
        | [] -> (* Nothing to aggregate *)
          if not (BlockMap.mem c !aggregate_map) then
            aggregate_map := BlockMap.add c
              (ref (BlockSet.singleton c)) !aggregate_map;
          path := c :: !path;
          true
        | ls -> begin (* Detect a loop here *)
          let bset = List.fold_left (fun acc c ->
            BlockSet.union acc (!(BlockMap.find c !aggregate_map))
          ) !(BlockMap.find c !aggregate_map) ls in
          BlockSet.iter (fun c ->
            aggregate_map := BlockMap.add c (ref bset) !aggregate_map
          ) bset;
          false
        end
      end
    in

    let stop_callback () = () in
    let contract_callback () = path := List.tl !path in
    let debug_log_callback = false in
    let log_callback state hint = if debug_log_callback
      then begin
        debug "%s | " hint;
        debug "%s | %s\n" state (List.fold_left (
          fun acc c-> BasicBlock.id c ^ " -> " ^ acc
        ) "" !path)
      end else ()
    in

    BGraph.dfs [Some entry; None] extend_callback
        contract_callback stop_callback log_callback;

    let _ = BlockMap.fold (fun k v total ->
      if BlockSet.mem k total then total
      else begin
        let aggro = BlockClosure.make !v aggro_map in
        BlockSet.iter (fun block ->
          aggro_map := BlockMap.add block (ref aggro) !aggro_map
        ) !v;
        BlockSet.union !v total
      end
    ) !aggregate_map BlockSet.empty
    in
    let aggro = !(BlockMap.find entry !aggro_map) in
    debug_aggro aggro;
    if entry_as_exit then flush stdout;
    aggro

  type 'a merge_point =
    | Merge of 'a
    | Diverge of 'a list
    | Dangle


  exception DivergeExitOfAggroSet

  let reach_merge_point mp b = match mp with
  | Merge b' -> BlockClosure.equal b' b
  | Dangle -> false
  | Diverge [] -> true
  | Diverge ls -> List.fold_left (fun acc b' ->
      acc || BlockClosure.equal b' b
    ) false ls

  let merge_to_string = function
  | Merge bgg -> "Merge | " ^ (BlockClosure.id bgg)
  | Dangle -> "Dangle"
  | Diverge ls -> List.fold_left (fun acc c -> acc ^ " " ^ BlockClosure.id c) "Diverge" ls

  (* get exits of an entry block aggro set *)
  let get_merge_point aggro entry_aggro =

    let path: BlockClosure.t list ref = ref [] in
    let mps: (BlockClosure.t list option) ref = ref None in
    let natual_exits = ref AggroSet.empty in
    let shrink = ref false in

    let upd_mp path =
      let r = match !mps with
      | None -> (List.rev path)
      | Some ts ->
        List.fold_left (fun acc (aggro:BlockClosure.t) ->
          if List.mem aggro ts then
            (* merge closure from new path *)
            acc @ [aggro]
          else acc
        ) [] path in
      let mps = List.fold_left (fun acc (aggro:BlockClosure.t) ->
        acc ^ (Printf.sprintf " %s " (BlockClosure.id aggro))
      ) "" r in
      debug "mps := < %s >\n" mps;
      Some r
    in

    (*
     * Since this is called after aggregrate
     * there should be no loop in the graph.
     *
     * Suppose that path = x_1, x_2, ... x_k,
     * then we add c at the begin of the path
     * new path = c, x_1, x_2, ..., x_k
     *)
    let extend_callback c =
      let pr_path () = if false then begin
          debug "path := < ";
          List.iter (fun (aggro:BlockClosure.t) ->
            debug " %s " (BlockClosure.id aggro)
          ) (List.rev !path);
          debug " -- end -- %s >\n" (BlockClosure.id aggro);
        end else () in

      if BlockClosure.equal c entry_aggro && List.length !path != 0
      then begin
        shrink := true;
        mps := upd_mp (List.tl (List.rev !path));
        pr_path ();
        false
      end else begin
        path := c :: !path;
        shrink := false;
        pr_path ();
        true
      end
    in

    let stop_callback () = () in
    let contract_callback () =
      if not !shrink then begin
        (* Update the merge point candidates *)
        shrink := true;
        let c = List.hd !path in
        mps := upd_mp (List.tl (List.rev !path));
        natual_exits := AggroSet.add c !natual_exits
      end else shrink := true;
      path := List.tl !path
    in

    (* let log_callback _ _ = () in *)
    let log_callback state hint = if false
      then begin
        debug "%s | " hint;
        debug "%s | %s\n" state (List.fold_left (
          fun acc c-> BlockClosure.id c ^ " -> " ^ acc
        ) "" !path)
      end else () in


    BlockClosureGraph.dfs [Some aggro] extend_callback
        contract_callback stop_callback log_callback;

    let m = match !mps with
    | None -> Dangle
    | Some ls -> match ls with
      | [] -> Diverge (AggroSet.elements !natual_exits)
      | bset :: _ -> Merge bset
    in
    debug "get merge point for %s --> %s\n" (BlockClosure.id aggro)
      (merge_to_string m);
    m

  (* -- end of get_merge_point -- *)

  let clean_exits es = ExitSet.elements @@ ExitSet.of_list es

  (*
   * Analysis a closure and make into a linear sequence of statements
   *)
  let rec trace closure previous entry_aggro merge_aggro
    (e, target) (translator:translator) =

    debug "++ trace %s in %s under %s ...\n" (BasicBlock.id target)
        (BlockClosure.id closure)
        (BlockClosure.id entry_aggro);

    let is_merge_aggro a =
        match merge_aggro with
        | None -> false
        | Some a' -> BlockClosure.equal a' a
    in

    let aggro = !(BlockClosure.find_aggro target closure) in
    let exit_aggros = get_merge_point aggro entry_aggro in
    debug ">>>> start trace_within >>>\n";
    let exits, statement =
        scope (fun () -> trace_within (e, target) aggro exit_aggros translator)
    in

    debug ">>>> finish trace_within >>>\n";
    let is_loop _ _ = false in
    let r = match exit_aggros with
    | Diverge _ (* blocks here are only for debug purpose *) ->
      if is_loop entry_aggro exits then
        exits, Statement.mkLoop [] statement
      else
        raise DivergeExitOfAggroSet

    | Dangle -> (exits, Statement.bind [] previous statement)

    | Merge aggro when is_merge_aggro aggro ->
      (* When we already reach the merge point *)
        exits, Statement.bind [] previous statement
    | Merge _ ->
      begin
        (* We have not reach the merge_point *)
        let lbs, exits = List.fold_left (fun (lbs, e) (label, branch) ->
            if false then (lbs, e)
            else begin
              if (BlockClosure.within_closure branch closure) then
                (lbs @ [label, branch], e)
              else
                (lbs, e @ [label, branch])
            end
          ) ([], []) exits in
        let exception_branchs = List.map (fun (l, _) ->
            ("exception", l, Statement.mkRaise 0)
          ) lbs in
        let stmts, exits = List.fold_left (fun (bs, es) (label, branch) ->
            let (e, next) = trace closure statement entry_aggro merge_aggro
                            (label, branch) translator in
            ["exception", label, next] @ bs, e @ es
          ) (exception_branchs, exits) lbs in
        let statement = merge_branchs previous stmts in
        (exits, statement)
      end
    in
    r

  and trace_blocks (_, previous) blocks aggro merge translator =
    let exists, stmts = List.fold_left (fun (es, stmts) (label, exp, b) ->
      debug "trace_blocks %s %s within %s ...\n" (Statement.Exp.to_string exp)
      (BasicBlock.id b)
      (BlockClosure.id aggro);
      let exists, stmt =
        if BlockClosure.within_closure b aggro then
          let next = BlockClosure.find_aggro b aggro in
          scope (fun () -> trace_within (exp, b) !next merge translator)
        else
        (* out of the aggro, thus we need to emit a raise here *)
          [(exp, b)], Statement.mkRaise 0
      in
      exists @ es, stmts @ [label, exp, stmt]
    ) ([],[]) blocks in
    let statement = merge_branchs previous stmts in
    (clean_exits exists), statement

  and merge_branchs previous stmts =
    match stmts with
    | [] -> previous
    | [_, _, s] -> Statement.bind [] previous s
    | branchs -> begin
        let catch = Statement.mkMutInd branchs in
        Statement.bind [] previous catch
      end

  (* Trace the entry block all the way to exit *)
  and trace_within (exp, entry) aggro merge translator
    : (Statement.Exp.t * BasicBlock.t) list * Statement.t
     =
    debug "-- trace_within %s %s ...\n" (BasicBlock.id entry)
      (BlockClosure.id aggro);
    if reach_merge_point merge aggro then begin
      debug "reach merge point\n";
      [exp, entry], Statement.mkFallThrough ()
    end else begin
      assert (BlockSet.mem entry aggro.blocks);
      match BlockSet.elements aggro.blocks with
      | [] -> assert false
      | [hd] -> (* hd must equal to entry *)
        let previous, exits = translator hd in
        let _ , exits = split_loop_exits hd exits in
          debug ">>> start trace blocks\n";
          let c = scope (fun _ -> trace_blocks previous exits aggro merge translator) in
          debug ">>> end trace blocks\n";
          c
      | _ -> begin
          let aggro = aggregate entry aggro.blocks true in
          scope (fun () -> trace aggro (Statement.mkFallThrough ())
            !(BlockClosure.find_aggro entry aggro)
            None (exp, entry) translator
          )
        end
    end
  (* ---- end of rec trace ---- *)

end
