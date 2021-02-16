open Exp

(* The Basic Blocks we would like to translate *)
module type Block = sig
  type elt
  type t
  val index: t -> int
  val id: t -> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val next: t -> t list
  val elements: t -> elt list
end

module type Translator = sig
  val translate: 'a -> 'b
end

module type Statement = sig
  module Exp: Exp
  type t
  val mkAssign: bool -> Exp.code -> Exp.t option -> Exp.t list -> t
  val mkMutInd: (string * Exp.t * t) list -> t
  val mkLoop: (Exp.t) list -> t -> t
  val mkFallThrough: unit -> t
  val mkDangling: unit -> t
  val mkRaise: int -> t
  val mkComment: string -> t
  val bind: Exp.t list -> t -> t -> t
  val emit: Emitter.t -> t -> unit
end

module Make:
  functor (S:Statement) (BasicBlock:Block with type elt = S.Exp.t)
  -> sig
  module BlockClosure: Block
  module Statement: (Statement with
    type Exp.t = BasicBlock.elt
    and type t= S.t
  )
  module BlockSet: (Set.S with type elt = BasicBlock.t)

  type error
  type 'a merge_point =
    | Merge of 'a
    | Diverge of 'a list
    | Dangle

  val aggregate: BasicBlock.t -> BlockSet.t -> bool -> BlockClosure.t

  val get_merge_point: BlockClosure.t -> BlockClosure.t
    -> BlockClosure.t merge_point

  type entry = string * Statement.Exp.t * BasicBlock.t
  type translator = BasicBlock.t -> ((Statement.Exp.t * Statement.t) * entry list)

  (* trace function: current closure -> previous_statement -> entry_aggro
    -> merge_aggro -> target block *)
  val trace: BlockClosure.t
    -> Statement.t
    -> BlockClosure.t (* Entry aggro *)
    -> BlockClosure.t option (* Merge aggro *)
    -> (Statement.Exp.t * BasicBlock.t)
    -> translator
    -> (Statement.Exp.t * BasicBlock.t) list * Statement.t

  val trace_within: (Statement.Exp.t * BasicBlock.t)
    -> BlockClosure.t
    -> BlockClosure.t merge_point
    -> translator
    -> (Statement.Exp.t * BasicBlock.t) list * Statement.t

  val debug_aggro: BlockClosure.t -> unit
  val emitter: Emitter.t ->  unit
end
