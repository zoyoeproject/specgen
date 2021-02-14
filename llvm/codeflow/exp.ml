module type Exp = sig
  type t
  type code
  val to_string : t -> string
  val code_to_string : code -> string
end


