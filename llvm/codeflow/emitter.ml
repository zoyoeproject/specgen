type t = {
  ident: string;
}

let indent t = {ident = "  " ^ t.ident}
let mkEmitter _ = {ident = ""}
let emitLine t x =
  Printf.printf "\n%s" t.ident;
  Printf.printf x

let emitEmptyLine _ =
  Printf.printf "\n"
