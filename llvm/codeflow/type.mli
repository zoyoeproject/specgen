(*
 * names that needs to be extened with path if necessary.
 *)

module type TypeId = sig
  type t
  val compare: t -> t -> int
end

module type Type = sig
  type 'a t
  type tname
  type fname
  (* Helper functions *)
  val mkNamed: tname -> 'a t
  val mkPrimitive: 'a -> 'a t
  val mkPtrType: 'a -> 'a t
  val mkAppType: 'a t -> 'a t array -> 'a t
  val mkRecordType: (fname * 'a t) array -> 'a t
end

module type Const = sig
  type t
  val to_string: t -> string
end


