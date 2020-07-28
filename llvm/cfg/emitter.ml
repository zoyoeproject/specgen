type t = {
  ident: string;
}

let indent t = {ident = "  " ^ t.ident}
let mkEmitter _ = {ident = ""}
let emitLine t x =
  Format.printf "\n%s" t.ident;
  Format.printf x
