type t = {
  ident: string;
  output: out_channel;
}

let indent t = {t with ident = "  " ^ t.ident}
let mkEmitter channel = {ident = ""; output = channel}
let emitLine t x =
  Printf.fprintf t.output "\n%s" t.ident;
  Printf.fprintf t.output x

let emitEmptyLine t =
  Printf.fprintf t.output "\n"
