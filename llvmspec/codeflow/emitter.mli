type t
val indent: t -> t
val emitLine: t -> ('a, out_channel, unit) format -> 'a
val emitEmptyLine: t -> unit
val mkEmitter: out_channel -> t
