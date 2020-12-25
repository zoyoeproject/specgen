open Utils
open TypeBuilder

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
  func_name = "llvm.dbg.value"

let emit_func_head lv =
  (* name should be used for file name *)
  (* let func_name = Llvm.value_name lv; *)
  Printf.printf "Definition\n";
  let func_ty = Llvm.element_type @@ Llvm.type_of lv in
  let ptypes = Llvm.param_types func_ty in
  let pargs = Llvm.params lv in
  let arg_type_pairs = Array.map2 (fun n t ->
      let typ = lltype_to_ctype t in
      let coqtyp = coq_type typ in
      Llvm.value_name n, coqtyp
  ) pargs ptypes in
  let rettyp = Llvm.return_type func_ty in
  Printf.printf "%s : %s :="
    (Array.fold_left (fun acc (n,t) ->
      acc ^ " (" ^ n ^ ":" ^ t ^")"
    ) "body" arg_type_pairs)
    (coq_type (lltype_to_ctype rettyp))

let translate_exits_br lli =
  match get_operands lli with
  | [|exp; v1; v2|] ->
    [(exp, Llvm.block_of_value v1); (exp, Llvm.block_of_value v2)]
  | [|exp; v1|] ->
    [(exp, Llvm.block_of_value v1)]
  | [|exp|] ->
    [(exp, Llvm.block_of_value exp)]
  | _ -> begin
      Printf.printf "%s\n" (Llvm.string_of_llvalue lli);
      assert false
    end

let translate_exits lli =
  let opcode = get_opcode lli in
  match opcode with
  | Br -> translate_exits_br lli
  | _ ->
    let succs = Array.to_list (Llvm.successors lli) in
    List.map (fun s -> Llvm.value_of_block s, s) succs

let is_debug_call opcode lli =
  let open Llvm.Opcode in
  match opcode with
  | Call -> begin
      Printf.printf "calling %s" (call_info lli);
      true
    end
  | _ -> false

let emit_llvm_inst latice lli =
  try
    let operands = get_operands lli in
    let opcode = get_opcode lli in
    if is_debug_call opcode lli then
       LlvmStatement.mkComment "debug info"
    else if Llvm.is_terminator lli then begin
      match opcode with
      | Ret -> LlvmStatement.mkAssign opcode None
          (List.map latice (Array.to_list operands))
      | _ -> LlvmStatement.mkFallThrough ()
    end else begin
      if is_assign opcode then
        LlvmStatement.mkAssign opcode
          (Some (latice lli)) (List.map latice (Array.to_list operands))
      else
        LlvmStatement.mkAssign opcode None
          (List.map latice (Array.to_list operands))
    end
  with e ->
    Printf.printf "\nEmit lli error: %s" (Llvm.string_of_llvalue lli);
    raise e
