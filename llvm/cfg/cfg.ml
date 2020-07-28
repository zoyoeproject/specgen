open Element

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

module type Statement = sig
  module Exp : Exp
  type 'a t
  val mkAssign: 'a Exp.t -> 'a Exp.t -> 'a t
  val mkLoad: 'a Exp.t -> 'a Exp.t -> 'a t
  val mkComment: string -> 'a t
  val mkFallThrough: unit -> 'a t
  val mkDangling: unit -> 'a t
  val emit: Emitter.t -> 'a t -> unit
end

module type Translator = sig
  val translate: 'a -> 'b
end

module Graph (B:Block) = struct
  let dfs entries extend_callback contract_callback stop_callback log_callback =
    let rec step_once ready_stack = begin
      assert (List.length ready_stack < 10);
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
  let with_in_closure t b = BlockMap.mem b !(t.aggregate_map)

  let find_aggro b t = BlockMap.find b !(t.aggregate_map)

  let compare x y = (index x) - (index y)
  let equal x y = ((index x) = (index y))
  let next bset =
    (* Format.printf " -- current_bset: %s --" (id bset); *)
    snd @@ BlockSet.fold (fun b acc ->
      let nexts = B.next b in
      List.fold_left (fun (total, s) n ->
        if BlockSet.mem n total then (total, s)
        else begin
          if with_in_closure bset n then
            (* Format.printf " <next of %s: %s> " (B.id b) (B.id n); *)
            let r = find_aggro n bset in
            (BlockSet.union (!r).blocks total, !r :: s)
          else (total, s)
        end
      ) acc nexts
    ) bset.blocks (bset.blocks,[])

  let elements bset = BlockSet.elements bset.blocks
  let element_set bset = bset.blocks

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
module Make (BasicBlock: Block) = struct
  module Exp = MakeExp
  module Statement = MakeStatement(Exp)
  module BlockSet = Set.Make(BasicBlock)
  module BlockMap = Map.Make(BasicBlock)
  module BGraph = Graph(BasicBlock)
  module BlockClosure = AggregateSet(BasicBlock)
  module BlockClosureGraph = Graph(BlockClosure)
  module AggroSet = Set.Make(BlockClosure)
  module AggroMap = Map.Make(BlockClosure)

  type error =
  | MultiEntry of BlockSet.t

  exception CFGError of (error * BlockClosure.t option)

  let debug aggro =
    BlockClosure.iter (fun aggo ->
      let aggs = BlockClosure.next aggo in
      let nexts = List.fold_left (fun acc a ->
        BlockClosure.id a ^ ";" ^ acc
      ) "" aggs in
      print_endline @@ (BlockClosure.id aggo) ^ " -> " ^ nexts
    ) aggro

  let emitter _ = raise @@ CFGError (MultiEntry BlockSet.empty, None)

  (*
   * Given an entry of type B.t compute entry' of type t.
   * So that entry' is a aggregate of entry and
   * there is no loop starting from entry' in Graph(t)
   *)
  let aggregate entry blockset entry_as_exit: BlockClosure.t =
    Format.printf "aggregating ...\n";
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
    let log_callback _ _ = () in
    (*
    let log_callback state hint =
      Format.printf "%s | " hint;
      Format.printf "%s | %s\n" state (List.fold_left (
        fun acc c-> BasicBlock.id c ^ " -> " ^ acc
      ) "" !path)
    in
    *)

    
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
    debug aggro;
    if entry_as_exit then flush stdout;
    aggro

  type 'a merge_point =
    | Merge of 'a
    | Diverge of 'a list
    | Dangle

  let merge_to_string = function
  | Merge bgg -> BlockClosure.id bgg
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
      Format.printf "mps := < ";
      List.iter (fun (aggro:BlockClosure.t) ->
        Format.printf " %s " (BlockClosure.id aggro)
      ) r;
      Format.printf ">\n";
      Some r
    in

    (*
     * Since this is called after aggregrate
     * there should be no loop in the graph.
     *
     * Suppose that path = x_1, x_2, ... x_k,
     * then we add c at the begin of the path
     * new patch = c, x_1, x_2, ..., x_k
     *)
    let extend_callback c =
      Format.printf "path := < ";
      List.iter (fun (aggro:BlockClosure.t) ->
        Format.printf " %s " (BlockClosure.id aggro)
      ) (List.rev !path);
      Format.printf " -- end -- %s >\n" (BlockClosure.id aggro);

      if BlockClosure.equal c entry_aggro && List.length !path != 0
      then begin
        shrink := true;
        mps := upd_mp (List.tl (List.rev !path));
        false
      end else begin
        path := c :: !path;
        shrink := false;
        true
      end
    in
    
    let stop_callback () = () in
    let contract_callback () =
      if not !shrink then begin
        (* Update the merge point candidates *)
        let c = List.hd !path in
        mps := upd_mp (List.tl (List.rev !path));
        natual_exits := AggroSet.add c !natual_exits
      end else shrink := true;
      path := List.tl !path
    in

    let log_callback _ _ = () in

    BlockClosureGraph.dfs [Some aggro] extend_callback
        contract_callback stop_callback log_callback;

    let m = match !mps with
    | None -> Dangle
    | Some ls -> match ls with
      | [] -> Diverge (AggroSet.elements !natual_exits)
      | bset :: _ -> Merge bset
    in
    Format.printf "get merge point for %s --> %s\n" (BlockClosure.id aggro)
      (merge_to_string m);
    m

  (* -- end of get_merge_point -- *)

  (*
   * Analysis a closure and make into a linear sequence of statements
   *)
  let rec trace closure previous entry_aggro merge_aggro target translator
    : (BlockSet.t * 'b Statement.t) =

    Format.printf "trace %s in %s ...\n" (BasicBlock.id target)
        (BlockClosure.id closure);

    let is_merge_aggro a =
        match merge_aggro with
        | None -> false
        | Some a' -> BlockClosure.equal a' a
    in

    let aggro = !(BlockClosure.find_aggro target closure) in
    let exit_aggros = get_merge_point aggro entry_aggro in
    let exits, (statement:'a Statement.t) =
        trace_within target aggro translator in
    let loop _ _ = false in
    let r = match exit_aggros with
    | Diverge bs (* blocks here are only for debug purpose *) ->
      (*
       * Non of the blocks should stay in the targetrent closure
       * except loop back
       *)
      let bs = List.fold_left (fun acc c ->
        BlockSet.union (BlockClosure.element_set c) acc
      ) BlockSet.empty bs in
      let bs = BlockSet.elements bs in
      let exits, branchs = List.fold_left (fun (es, ss) b ->
          let (exits, statement) =
          if (BlockClosure.with_in_closure closure b) then
            trace closure (Statement.mkFallThrough ()) entry_aggro
                (Some aggro) b translator
          else
            (* something not in closure, contradict with
             * merge assumption *)
            assert false
          in
          BlockSet.union es exits, ss @ [(Statement.Exp.mkUnit (), statement)]
        ) (BlockSet.empty, []) bs in
      let statement = match branchs with
      | [] -> Statement.bind None previous statement
      | [h] -> Statement.bind None previous @@ Statement.bind None statement (snd h)
      | _ -> begin
          let catch = Statement.mkMutInd branchs in
          let statement = Statement.bind None statement catch in
          Statement.bind None previous statement
        end
      in
      if loop entry_aggro exits then
        exits, Statement.mkLoop [] statement
      else
        exits, statement

    | Dangle -> (exits, Statement.bind None previous statement)

    | Merge aggro when is_merge_aggro aggro ->
      (* When we already reach the merge point *)
        exits, Statement.bind None previous statement
    | Merge aggro ->
      begin
        (* We have not reach the merge_point *)
        let exits, branchs = List.fold_left (fun (es, ss) b ->
          let (exits, statement) =
          if (BlockClosure.with_in_closure closure b) then
            trace closure (Statement.mkFallThrough ()) entry_aggro
                (Some aggro) b translator
          else
            (* something not in closure, contradict with
             * merge assumption *)
            assert false
          in
          BlockSet.union es exits, ss @ [(Statement.Exp.mkUnit (), statement)]
        ) (BlockSet.empty, []) (BlockClosure.elements aggro) in
        let statement = match branchs with
        | [] -> Statement.bind None previous statement
        | [h] -> Statement.bind None previous @@ Statement.bind None statement (snd h)
        | _ -> begin
            let catch = Statement.mkMutInd branchs in
            Statement.bind None (Statement.bind None previous statement) catch
          end
        in (exits, statement)
      end
    in
    r

  (* Trace the entry block all the way to exit*)
  and trace_within entry aggro translator =
    Format.printf "trace %s within %s ...\n" (BasicBlock.id entry) (BlockClosure.id aggro);
    assert (BlockSet.mem entry aggro.blocks);
    match BlockSet.elements aggro.blocks with
    | [] -> assert false
    | [hd] -> (* hd must equal to entry *)
      BlockSet.of_list (BasicBlock.next hd), translator hd
    | _ -> begin
        let aggro = aggregate entry aggro.blocks true in
        trace aggro (Statement.mkFallThrough ())
          !(BlockClosure.find_aggro entry aggro)
          None entry translator
      end
  (* ---- end of rec trace ---- *)

end
