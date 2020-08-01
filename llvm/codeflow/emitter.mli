type t
val indent: t -> t
val emitLine: t -> ('a, Format.formatter, unit) format -> 'a
val mkEmitter: unit -> t
