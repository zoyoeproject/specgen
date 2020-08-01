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

module type Exp = sig
  type 'a t
end

module type Statement = sig
  module Exp: Exp
  type 'a t
  val mkAssign: 'a Exp.t -> 'a Exp.t -> 'a t
  val mkLoad: 'a Exp.t -> 'a Exp.t -> 'a t
  val mkMutInd: ('a Exp.t * 'a t) list -> 'a t
  val mkLoop: ('a Exp.t) list -> 'a t -> 'a t
  val mkFallThrough: unit -> 'a t
  val mkDangling: unit -> 'a t
  val mkRaise: int -> 'a t
  val mkComment: string -> 'a t
  val bind: ('a Exp.t) option -> 'a t -> 'a t -> 'a t
  val emit: Emitter.t -> 'a t -> unit
end

module Make:
  functor (BasicBlock:Block) -> sig
  module BlockClosure: Block
  module Statement: Statement
  module BlockSet: (Set.S with type elt = BasicBlock.t)

  type error
  type 'a merge_point =
    | Merge of 'a
    | Diverge of 'a list
    | Dangle

  val aggregate: BasicBlock.t -> BlockSet.t -> bool -> BlockClosure.t

  val get_merge_point: BlockClosure.t -> BlockClosure.t
    -> BlockClosure.t merge_point

  (* trace function: current closure -> previous_statement -> entry_aggro
    -> merge_aggro -> target block *)
  val trace: BlockClosure.t -> 'a Statement.t
    -> BlockClosure.t (* Entry aggro *)
    -> BlockClosure.t option (* Merge aggro *)
    -> BasicBlock.t
    -> (BasicBlock.t -> 'a Statement.t)
    -> BlockSet.t * 'a Statement.t

  val debug: BlockClosure.t -> unit
  val emitter: Emitter.t ->  unit
end
