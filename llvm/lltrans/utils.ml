open Codeflow

exception UnsupportOpcode

let op_to_string op_code =
  let open Llvm.Opcode in
  match op_code with
  | Ret -> "ret"
  | Br -> "br"
  | Switch -> assert false
  | IndirectBr -> assert false
  | Invoke -> assert false
  | Invalid2 -> assert false
  | Unreachable -> assert false
  | Add -> "wadd"
  | FAdd -> "fadd"
  | Sub -> "wsub"
  | FSub -> "fsub"
  | Mul -> "wmul"
  | FMul -> "fmul"
  | UDiv -> "udiv"
  | SDiv -> "sdiv"
  | FDiv -> "fdiv"
  | URem -> "urem"
  | SRem -> "srem"
  | FRem -> "frem"
  | Shl -> "wshl"
  | LShr -> "wlshr"
  | AShr -> "washr"
  | And -> "wand"
  | Or -> "wor"
  | Xor -> "wxor"
  | Alloca -> "alloca"
  | Load -> assert false
  | Store -> "set_obj"
  | GetElementPtr -> assert false
  | ICmp -> "icomp"
  | FCmp -> "fcomp"
  | Select -> "if"
  | PHI -> "phi"
  | _ -> raise UnsupportOpcode

module type IndexMap = sig
  val get_block_id : string -> int
end

module LlvmValue = struct
  type t = Llvm.llvalue
  type code = Llvm.Opcode.t
  let code_to_string = op_to_string
  let to_string a = Llvm.value_name a
  let compare a b = String.compare (Llvm.value_name a) (Llvm.value_name b)
end

module LlvmStatement = Codeflow.Element.MakeStatement (LlvmValue)

module BasicBlock (Index: IndexMap)
  : (Cfg.Block with type elt = LlvmValue.t
    and type t = Llvm.llbasicblock) = struct
  type elt = LlvmValue.t
  type t = Llvm.llbasicblock
  let id b = Llvm.value_name (Llvm.value_of_block b)
  let index b = Index.get_block_id (id b)
  let compare a b = (index a) - (index b)
  let equal b a = (index a) = (index b)
  let elements b =
    Llvm.fold_left_instrs (fun acc lli -> acc @ [lli]) [] b
  let next b =
    let llterminator = Llvm.block_terminator b in
    Array.to_list (Llvm.successors (Option.get llterminator))
end

