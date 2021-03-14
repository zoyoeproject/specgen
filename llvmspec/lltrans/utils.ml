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

type name =
  | Anonymous
  | Name of string

let string_of_name na = match na with
  | Anonymous -> assert false
  | Name str -> str

let mkName stropt = match stropt with
  | None -> Anonymous
  | Some str -> Name str

let is_constant llvalue =
  match Llvm.classify_value llvalue with
  | ConstantInt -> true
  | NullValue -> true
  | ConstantExpr -> true
  | _ -> false

let is_phi_op op_code =
  let open Llvm.Opcode in
  match op_code with
  | PHI -> true
  | _ -> false

let is_phi_value llvalue =
  match Llvm.classify_value llvalue with
  | Instruction _ -> true
  | Argument -> true
  | _ -> false

let value_to_string to_string llvalue =
  match Llvm.classify_value llvalue with
  | NullValue -> "NullValue"
  | Argument -> Llvm.value_name llvalue
  | ConstantInt -> "(" ^ Llvm.string_of_llvalue llvalue ^ ")"
  | ConstantExpr -> "cexpr " ^ (Llvm.string_of_llvalue llvalue)
  | Instruction _ -> to_string llvalue
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

let update_phi_lattice upd_lattice lli =
  let opcode = get_opcode lli in
  match opcode with
  | PHI -> begin
      let incoming = Llvm.incoming lli in
      List.iter (fun (src, b) ->
        upd_lattice b src lli
      ) incoming
    end
  | _ -> ()

module LlvmStatement (LlvmValue: Exp.Exp) = Codeflow.Element.MakeStatement (LlvmValue)

module BasicBlock (Index: IndexMap) (LlvmValue: Exp.Exp with type t = Llvm.llvalue)
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

