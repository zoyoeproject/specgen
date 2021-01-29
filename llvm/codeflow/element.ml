module Id = String
module Name = String
module TypeMap = Map.Make(Id)

module type Exp = sig
  type t
  type code
  val to_string : t -> string
  val code_to_string: code -> string
end

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

  let bind v arg app = match arg with
     | FallThrough -> app
     | _ -> Bind (v, arg, app)
  let rec emit emitter s = match s with
    | Comment s -> Emitter.emitLine emitter "(* %s *)" s
    | Bind (_, s1, s2) -> emit emitter s1; emit emitter s2
    | MutInd ls ->
      let _, cond, _ = List.hd ls in
      Emitter.emitLine emitter "let _ = match %s with" (Exp.to_string cond);
      let emitter = Emitter.indent emitter in
      ignore @@ List.map (fun (e,_,s) ->
        Emitter.emitLine emitter "%s =>" e;
        let emitter' = Emitter.indent emitter in
        emit emitter' s
      ) ls;
      Emitter.emitLine emitter "in"
    | Loop (_, s) -> begin
      Emitter.emitLine emitter "%s" "let ctx = loop ctx do";
      let emitter' = Emitter.indent emitter in
      emit emitter' s;
      Emitter.emitLine emitter "%s" "end loop in"
      end
    | Raise c -> Emitter.emitLine emitter "raise %d" c
    | FallThrough -> Emitter.emitLine emitter "(* fallthrough *)"
    | Dangling -> Emitter.emitLine emitter "dangling"
    | Load (x, y) -> Emitter.emitLine emitter "%s <- load %s;"
        (Exp.to_string x) (Exp.to_string y)
    | Assign (op, lhs, ops) ->
        let rhs = List.fold_left (fun acc operand ->
              acc ^ " " ^  (Exp.to_string operand)
          ) (Exp.code_to_string op) ops in
        match lhs with
        | Some lhs -> Emitter.emitLine emitter "%s ?= %s;"
            (Exp.to_string lhs) rhs
        | None -> Emitter.emitLine emitter "%s;;" rhs

end
