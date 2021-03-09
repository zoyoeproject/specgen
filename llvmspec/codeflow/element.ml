module Id = String
module Name = String
module TypeMap = Map.Make(Id)
open Exp

module MakeStatement (E:Exp) = struct
  module Exp = E
  type t =
    | Comment of string
    | Assign of (bool * Exp.code * Exp.t option * Exp.t list)
    | MutInd of (string * Exp.t * t) list
    | Loop of (Exp.t list * t)
    | Bind of (Exp.t list * t * t)
    | Return of Exp.t array
    | FallThrough (* empty statement *)
    | Dangling (* non'a tinate statement *)
    | Raise of int

  let mkAssign b c lhs ops = Assign (b, c, lhs, ops)
  let mkMutInd cases = MutInd cases
  let mkLoop ctx ts = Loop (ctx, ts)
  let mkFallThrough _ = FallThrough
  let mkDangling _ = Dangling
  let mkRaise i = Raise i
  let mkComment c = Comment c
  let mkReturn c = Return c

  let string_of_ctx ctx =
    match ctx with
    | [] -> "()"
    | hd :: tl ->
      let inner = List.fold_left (fun acc c ->
        acc ^ ", " ^ (Exp.to_string c)
      ) (Exp.to_string hd) tl in
      "(" ^ inner ^ ")"

  let rec bind v arg app = match arg with
     | FallThrough -> app
     | Bind (x, s1, s2) -> Bind (x, s1, bind v s2 app)
     | _ -> Bind (v, arg, app)

  let rec collect_context ctx s =
    match s with
    | Bind (v, _, _) -> v @ ctx
    | MutInd ((_,_,s) :: tl)-> begin
        List.fold_left (fun ctx (_,_,s) ->
          collect_context ctx s
        ) (collect_context ctx s) tl
      end
    | _ -> ctx

  let add_context ctx v =
    let add_one ctx c =
      try
        let _ = List.iter (fun e ->
          if e == c then raise (Failure "Found") else ()
        ) ctx in
        raise Not_found
      with
        | Failure _ -> ctx
        | Not_found -> c :: ctx
    in
    List.fold_left (fun acc v -> add_one acc v) ctx v

  let rec emit_ctx emitter ctx s =
    match s with
    | Comment s -> Emitter.emitLine emitter "(* %s *)" s; ctx
    | Bind (v, s1, s2) -> begin
        let ctx = add_context ctx v in
        let ctx = emit_ctx emitter ctx s1 in
        emit_ctx emitter ctx s2
      end
    | MutInd ls -> begin
        Emitter.emitLine emitter "(* context: %s *)" (string_of_ctx ctx);
        let ctx = collect_context ctx s in
        let _, cond, _ = List.hd ls in
        Emitter.emitLine emitter "__context %%= match %s with"
          (*string_of_ctx ctx*) (Exp.to_string cond);
        let emitter' = Emitter.indent emitter in
        ignore @@ List.map (fun (e,_,s) ->
          Emitter.emitLine emitter' "| %s =>" e;
          let emitter' = Emitter.indent emitter' in
          emit_ctx emitter' ctx s
        ) ls;
        Emitter.emitLine emitter "end;";
        Emitter.emitLine emitter "let %s = __context in" (string_of_ctx ctx);
        ctx
      end
    | Loop (lctx, s) -> begin
        Emitter.emitLine emitter "%s" "let ctx = loop ctx do";
        let emitter' = Emitter.indent emitter in
        let ctx = emit_ctx emitter' (lctx @ ctx) s in
        Emitter.emitLine emitter "%s" "end loop in";
        ctx
      end
    | Raise c -> Emitter.emitLine emitter "raise %d" c; ctx
    | FallThrough -> begin
        Emitter.emitLine emitter "ret %s" (string_of_ctx ctx);
        Emitter.emitLine emitter "(* fallthrough *)";
        ctx
      end
    | Dangling -> Emitter.emitLine emitter "dangling"; ctx
    | Return op -> begin
        let r = if Array.length(op) = 0 then "()"
          else (Exp.to_string op.(0))
        in
        Emitter.emitLine emitter "%s" ("ret " ^ r);
        ctx
      end
    | Assign (pure, op, lhs, ops) -> begin
        let rhs = List.fold_left (fun acc operand ->
              acc ^ " " ^  (Exp.to_string operand)
          ) (Exp.code_to_string op) ops in
        let _ = match lhs with
          | Some lhs -> Emitter.emitLine emitter "%s %%= %s;"
              (Exp.to_string lhs)
              (if pure then "ret (" ^ rhs ^ ")" else rhs)
          | None -> Emitter.emitLine emitter "%s;;" rhs
        in ctx
      end

  let emit emitter s = ignore @@ emit_ctx emitter [] s

end
