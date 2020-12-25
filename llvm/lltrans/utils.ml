open Codeflow

exception UnsupportOpcode of Llvm.Opcode.t
exception UnsupportOperand of Llvm.llvalue
exception UnsupportType of Llvm.lltype

let () =
  Printexc.register_printer
    (function
      | UnsupportType lltyp -> Some (Printf.sprintf "UnsupportType[%s]" (Llvm.string_of_lltype lltyp))
      | _ -> None
    )

let () =
  Printexc.register_printer
    (function
      | UnsupportOperand llvalue -> Some (Printf.sprintf "UnsupportOperand[%s]" (Llvm.string_of_llvalue llvalue))
      | _ -> None
    )

external call_info: Llvm.llvalue -> string = "llvm_call_info"

type name =
  | Anonymous
  | Name of string

let string_of_name na = match na with
  | Anonymous -> assert false
  | Name str -> str

let mkName stropt = match stropt with
  | None -> Anonymous
  | Some str -> Name str

let value_to_string llvalue =
  match Llvm.classify_value llvalue with
  | NullValue -> "NullValue"
  | Argument -> Llvm.value_name llvalue
  | ConstantInt -> "(" ^ Llvm.string_of_llvalue llvalue ^ ")"
  | ConstantExpr -> "cexpr " ^ (Llvm.string_of_llvalue llvalue)
  | Instruction _ -> (Llvm.value_name llvalue)
  | BasicBlock -> "basic_block"
(* Unsupported operand
  | BasicBlock
  | InlineAsm
  | MDNode
  | MDString
  | BlockAddress
  | ConstantAggregateZero
  | ConstantArray
  | ConstantDataArray
  | ConstantDataVector
  | ConstantFP
  | ConstantPointerNull
  | ConstantStruct
  | ConstantVector
  | Function
  | GlobalAlias
  | GlobalIFunc
  | GlobalVariable
  | UndefValue
  | Instruction of Opcode.t
*)
  | _ -> raise (UnsupportOperand llvalue)



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
  | Load -> "get_obj"
  | Store -> "set_obj"
  | GetElementPtr -> "field"
  | ICmp -> "icomp"
  | FCmp -> "fcomp"
  | Select -> "if"
  | PHI -> "phi"

  | Trunc (* Cast Operators *)
  | ZExt -> "zext"
  | SExt -> "sext"
(*
  | FPToUI
  | FPToSI
  | UIToFP
  | SIToFP
  | FPTrunc
  | FPExt
  | PtrToInt
  | IntToPtr
  | BitCast
*)

  | _ -> raise (UnsupportOpcode op_code)

module type IndexMap = sig
  val get_block_id : string -> int
end

let get_opcode lli =
  match Llvm.classify_value lli with
  | Instruction opcode -> opcode
  | _ -> assert false

let get_operands lli =
  Array.init (Llvm.num_operands lli) (fun n -> Llvm.operand lli n)

let update_phi_lattice lattice init lli =
  let opcode = get_opcode lli in
  let operands = get_operands lli in
  match opcode with
  | PHI -> Array.fold_left (fun l src -> lattice l src lli) init operands
  | _ -> init

module LlvmValue = struct
  type t = Llvm.llvalue
  type code = Llvm.Opcode.t
  let code_to_string = op_to_string
  let to_string a = value_to_string a
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

