module Id = String
module Name = String
module TypeMap = Map.Make(Id)
open Exp

module MakeStatement (E:Exp) = struct
  module Exp = E
  type t =
    | Comment of string
    | Assign of (Exp.code * Exp.t option * Exp.t list)
    | Load of (Exp.t * Exp.t)
    | MutInd of (string * Exp.t * t) list
    | Loop of (Exp.t list * t)
    | Bind of (Exp.t list * t * t)
    | FallThrough (* empty statement *)
    | Dangling (* non'a tinate statement *)
    | Raise of int

  let mkAssign c lhs ops = Assign (c, lhs, ops)
  let mkLoad v ptr = Load (v, ptr)
  let mkMutInd cases = MutInd cases
  let mkLoop ctx ts = Loop (ctx, ts)
  let mkFallThrough _ = FallThrough
  let mkDangling _ = Dangling
  let mkRaise i = Raise i
  let mkComment c = Comment c

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

  let rec emit_ctx emitter ctx s = match s with
    | Comment s -> Emitter.emitLine emitter "(* %s *)" s; ctx
    | Bind (v, s1, s2) -> begin
        let ctx = v @ ctx in
        let ctx = emit_ctx emitter ctx s1 in
        emit_ctx emitter ctx s2
      end
    | MutInd ls -> begin
        let _, cond, _ = List.hd ls in
        Emitter.emitLine emitter "let _ = match %s with" (Exp.to_string cond);
        let emitter = Emitter.indent emitter in
        ignore @@ List.map (fun (e,_,s) ->
          Emitter.emitLine emitter "%s =>" e;
          let emitter' = Emitter.indent emitter in
          emit_ctx emitter' ctx s
        ) ls;
        Emitter.emitLine emitter "in";
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
        Emitter.emitLine emitter "%s" (string_of_ctx ctx);
        Emitter.emitLine emitter "(* fallthrough *)";
        ctx
      end
    | Dangling -> Emitter.emitLine emitter "dangling"; ctx
    | Load (x, y) -> begin
        Emitter.emitLine emitter "%s <- load %s;"
          (Exp.to_string x) (Exp.to_string y);
        ctx
      end
    | Assign (op, lhs, ops) -> begin
        let rhs = List.fold_left (fun acc operand ->
              acc ^ " " ^  (Exp.to_string operand)
          ) (Exp.code_to_string op) ops in
        let _ = match lhs with
          | Some lhs -> Emitter.emitLine emitter "%s ?= %s;"
              (Exp.to_string lhs) rhs
          | None -> Emitter.emitLine emitter "%s;;" rhs
        in ctx
      end

  let emit emitter s = ignore @@ emit_ctx emitter [] s

end
