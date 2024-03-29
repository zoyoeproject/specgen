open Utils
open TypeBuilder
open Codeflow

type llvalue_lattice = Llvm.llvalue -> Llvm.llvalue

let is_pure op_code =
  let open Llvm.Opcode in
  match op_code with
  | Add | FAdd | Sub | FSub | Mul | FMul
  | UDiv | SDiv | FDiv | URem | SRem | FRem
  | Shl -> true | LShr | AShr | And | Or
  | Xor -> true
  | Alloca -> false
  | Load -> false
  | GetElementPtr -> true (* This is the only way to get filed info *)
  | Ret -> assert false
  | ICmp -> true
  | FCmp -> true
  | PHI -> true
  | Select -> true
  | Trunc -> true (* Cast Operators *)
  | ZExt -> true
  | SExt -> true
  | FPToUI -> true
  | FPToSI -> true
  | UIToFP -> true
  | SIToFP -> true
  | FPTrunc -> true
  | FPExt -> true
  | PtrToInt -> true
  | IntToPtr -> true
  | BitCast -> true
  | _ -> false

let is_assign op_code =
  let open Llvm.Opcode in
  match op_code with
  | Add -> true
  | FAdd -> true
  | Sub -> true
  | FSub -> true
  | Mul -> true
  | FMul -> true
  | UDiv -> true
  | SDiv -> true
  | FDiv -> true
  | URem -> true
  | SRem -> true
  | FRem -> true
  | Shl -> true
  | LShr -> true
  | AShr -> true
  | And -> true
  | Or -> true
  | Xor -> true
  | Alloca -> true
  | Load -> true
  | GetElementPtr -> true (* This is the only way to get filed info *)
  | Ret -> assert false
  | ICmp -> true
  | FCmp -> true
  | PHI -> true
  | Select -> true
  | Trunc -> true (* Cast Operators *)
  | ZExt -> true
  | SExt -> true
  | FPToUI -> true
  | FPToSI -> true
  | UIToFP -> true
  | SIToFP -> true
  | FPTrunc -> true
  | FPExt -> true
  | PtrToInt -> true
  | IntToPtr -> true
  | BitCast -> true
  | _ -> false


let is_debug_fun_decl lv =
  let func_name = Llvm.value_name lv in
  let ns = String.split_on_char '.' func_name in
  match ns with
  | "llvm" :: "dbg" :: _ -> true
  | _ -> false

let emit_func_head emitter lv =
  (* name should be used for file name *)
  (* let func_name = Llvm.value_name lv; *)
  Codeflow.Emitter.emitLine emitter "Definition";
  let func_ty = Llvm.element_type @@ Llvm.type_of lv in
  let ptypes = Llvm.param_types func_ty in
  let pargs = Llvm.params lv in
  let arg_type_pairs = Array.map2 (fun n t ->
      let typ = lltype_to_ctype t in
      let coqtyp = coq_type typ in
      Llvm.value_name n, coqtyp
  ) pargs ptypes in
  let rettyp = Llvm.return_type func_ty in
  Codeflow.Emitter.emitLine emitter "%s: monad (state CType) %s :="
    (Array.fold_left (fun acc (n,t) ->
      acc ^ " (" ^ n ^ ":" ^ t ^")"
    ) "body" arg_type_pairs)
    (coq_type (lltype_to_ctype rettyp))

let translate_exits_br lli =
  match get_operands lli with
  | [|exp; v1; v2|] ->
    [("true", exp, Llvm.block_of_value v1); ("false", exp, Llvm.block_of_value v2)]
  | [|exp; v1|] ->
    [("cond", exp, Llvm.block_of_value v1)]
  | [|exp|] ->
    [("cond", exp, Llvm.block_of_value exp)]
  | _ -> begin
      Printf.printf "%s\n" (Llvm.string_of_llvalue lli);
      assert false
    end

(*
 * val translate_exits: Llvm.llvalue ->
 *   (string * Llvm.llvalue * Llvm.llbasicblock) list
 *)
let translate_exits lli =
  let opcode = get_opcode lli in
  match opcode with
  | Br -> translate_exits_br lli
  | _ ->
    let succs = Array.to_list (Llvm.successors lli) in
    List.map (fun s -> "cond", (Llvm.value_of_block s), s) succs

let is_debug_call opcode _ =
  let open Llvm.Opcode in
  match opcode with
  | Call -> begin
      true
    end
  | _ -> false

let first_operand_has_struct_type operands =
   let typ = Llvm.type_of operands.(0) in
   match Llvm.classify_type typ with
   | Llvm.TypeKind.Pointer -> begin
       let coq_typ = TypeBuilder.lltype_to_ctype
         (Llvm.element_type typ) in
       is_struct_type coq_typ
     end
   | _ -> false

module Translator (LlvmValue: Exp.Exp
    with type t = Llvm.llvalue
    and type code = string
  ) = struct

  module LlvmStatement = LlvmStatement(LlvmValue)

  (*
   * val emit_llvm_inst:
   *   (Llvm.llvalue -> Llvm.llvalue) -> Llvm.llvalue -> LlvmStatement.t
   *)
  let emit_llvm_inst latice lli =
    try
      let operands = get_operands lli in
      let opcode = get_opcode lli in
      if is_debug_call opcode lli then begin
         Array.iter (fun operand ->
           ignore @@ Option.map (fun mn ->
             Llvmdinfo.register_dinfo mn
           ) (Llvmdinfo.llvalue_to_metadata operand)
         ) operands;
         LlvmStatement.mkComment "llvm debug info detected"
      end else if Llvm.is_terminator lli then begin
        match opcode with
        | Ret -> LlvmStatement.mkReturn (Array.map latice operands)
        | _ -> LlvmStatement.mkFallThrough ()
      end else if is_phi_op opcode then begin
        ignore (Array.map latice operands); (* FIXME: Still has to track all types *)
        LlvmStatement.mkFallThrough ()
      end else begin
        if is_assign opcode then
          match opcode with
          | GetElementPtr when first_operand_has_struct_type operands -> begin
            let ltype = Llvm.type_of operands.(0) in
            let idx = Option.get @@ Llvm.int64_of_const operands.(2) in
            let field_op = TypeBuilder.get_field_op (Llvm.element_type ltype)
              (Int64.to_int idx)
            in
            LlvmStatement.mkAssign (true) (field_op)
              (Some (latice lli)) [latice operands.(0)]
            end
          | _ -> LlvmStatement.mkAssign (is_pure opcode) (op_to_string opcode)
            (Some (latice lli)) (List.map latice (Array.to_list operands))
        else
          LlvmStatement.mkAssign (is_pure opcode) (op_to_string opcode) None
            (List.map latice (Array.to_list operands))
      end
    with e ->
      Printf.printf "\nEmit lli error: %s" (Llvm.string_of_llvalue lli);
      raise e

  let translator var_lattice phi_table llblock =
    let llvalue = Llvm.value_of_block llblock in
    let claims = match Hashtbl.find_opt phi_table llblock with
      | Some ls -> ls
      | None -> []
    in
    let statement = List.fold_left (fun acc (v, exp) ->
      let s = LlvmStatement.mkAssign false "ret" (Some v) [exp] in
      LlvmStatement.bind [v] s acc
    ) (LlvmStatement.mkFallThrough ()) (List.rev claims)
    in
    let statement = Llvm.fold_left_instrs (fun acc v ->
      let stmt = emit_llvm_inst var_lattice v in
      LlvmStatement.bind [] acc stmt
    ) statement llblock
    in
    let llterminator = Llvm.block_terminator llblock in
    let exists = translate_exits (Option.get llterminator) in
    (llvalue, statement), exists

end
