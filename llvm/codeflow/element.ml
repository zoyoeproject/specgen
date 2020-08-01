module Id = String
module Name = String
module TypeMap = Map.Make(Id)

module type Exp = sig
  type vname
  type fname
  type 'a t

  (* Helper functions *)
  val mkVar : vname -> 'a t
  val mkConstant : 'a -> 'a t
  val mkRef : 'a t -> 'a t
  val mkDeref : 'a t -> 'a t
  val mkApp : 'a t -> 'a t array -> 'a t
  val mkOffset : fname -> 'a t -> 'a t
  val mkUnit : unit -> 'a t
  val to_string : 'a t -> string
end

module MakeExp : Exp = struct
  type vname = Name.t
  type fname = Name.t
  type 'a t =
    | UNIT
    | VAR of vname
    | CONSTANT of 'a
    | REF of 'a t
    | DEREF of 'a t
    | OFFSET of (fname * 'a t)
    | APP of 'a t * ('a t array)

  (* Helper functions *)
  let mkVar t = VAR t
  let mkConstant c = CONSTANT c
  let mkRef t = REF t
  let mkDeref t = DEREF t
  let mkOffset f t = OFFSET (f, t)
  let mkApp t ta = APP (t, ta)
  let mkUnit () = UNIT
  let to_string _ = "Unimplemented"

end

module MakeStatement (E:Exp) = struct
  module Exp = E
  type 'a t =
    | Comment of string
    | Assign of ('a Exp.t * 'a Exp.t)
    | Load of ('a Exp.t * 'a Exp.t)
    | MutInd of ('a Exp.t * 'a t) list
    | Loop of (('a Exp.t) list * 'a t)
    | Bind of (('a Exp.t) option * 'a t * 'a t)
    | FallThrough (* empty statement *)
    | Dangling (* non'a tinate statement *)
    | Raise of int

  let mkAssign x y = Assign (x, y)
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
    | Comment s -> Emitter.emitLine emitter "%s" s
    | Bind (_, s1, s2) -> emit emitter s1; emit emitter s2
    | MutInd ls ->
      Emitter.emitLine emitter "let _ = cases";
      let emitter = Emitter.indent emitter in
      ignore @@ List.map (fun (_,s) ->
        let emitter' = Emitter.indent emitter in
        Emitter.emitLine emitter "%s" "case _ then";
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
    | FallThrough -> Emitter.emitLine emitter "fallthrough"
    | Dangling -> Emitter.emitLine emitter "dangling"
    | Load (x, y) -> Emitter.emitLine emitter "%s <- load %s;"
        (Exp.to_string x) (Exp.to_string y)
    | Assign (x, y) -> Emitter.emitLine emitter "%s <- ret %s;"
        (Exp.to_string x) (Exp.to_string y)

end
