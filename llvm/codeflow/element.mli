module type Exp = sig
  type t
  val to_string : t -> string
end

module MakeStatement:
  functor (E:Exp) -> sig
  module Exp : (Exp with type t = E.t)
  type t
  val mkAssign: Exp.t -> Exp.t -> t
  val mkLoad: Exp.t -> Exp.t -> t
  val mkMutInd: (Exp.t * t) list -> t
  val mkLoop: Exp.t list -> t -> t
  val mkFallThrough: unit -> t
  val mkDangling: unit -> t
  val mkRaise: int -> t
  val mkComment: string -> t
  val bind: Exp.t option -> t -> t -> t
  val emit: Emitter.t -> t -> unit
end


