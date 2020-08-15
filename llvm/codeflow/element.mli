module type Exp = sig
  type t
  type code
  val to_string : t -> string
  val code_to_string : code -> string
end

module MakeStatement:
  functor (E:Exp) -> sig
  module Exp : (Exp with type t = E.t and type code = E.code)
  type t
  val mkAssign: Exp.code -> Exp.t option -> Exp.t list -> t
  val mkLoad: Exp.t -> Exp.t -> t
  val mkMutInd: (Exp.t * t) list -> t
  val mkLoop: Exp.t list -> t -> t
  val mkFallThrough: unit -> t
  val mkDangling: unit -> t
  val mkRaise: int -> t
  val mkComment: string -> t
  val bind: Exp.t list -> t -> t -> t
  val emit: Emitter.t -> t -> unit
end


