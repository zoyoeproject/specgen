open Exp

module MakeStatement:
  functor (E:Exp) -> sig
  module Exp : (Exp with type t = E.t and type code = E.code)
  type t
  val mkAssign: bool -> Exp.code -> Exp.t option -> Exp.t list -> t
  val mkMutInd: (string * Exp.t * t) list -> t
  val mkLoop: Exp.t list -> t -> t
  val mkFallThrough: unit -> t
  val mkDangling: unit -> t
  val mkRaise: int -> t
  val mkReturn: Exp.t array -> t
  val mkComment: string -> t
  val bind: Exp.t list -> t -> t -> t
  val emit: Emitter.t -> t -> unit
end


