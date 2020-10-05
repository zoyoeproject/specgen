type t
val indent: t -> t
val emitLine: t -> ('a, out_channel, unit) format -> 'a
val mkEmitter: unit -> t
