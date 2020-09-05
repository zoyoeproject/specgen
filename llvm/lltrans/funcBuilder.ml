open Utils

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

type c_type =
  | Void
  | Int
  | Ref of c_type
  | Struct of name
  | Function of name

(*
let rec indicator_of ctype =
  match ctype with
  | Void -> "Unit"
  | Int -> "Int"
  | Ref ctype -> "Ref " ^ indicator_of ctype
  | Struct na -> "T_" ^ string_of_name na
  | Function na -> "F_" ^ string_of_name na
*)

let rec coq_type ctype =
  match ctype with
  | Void -> "void"
  | Int -> "Z"
  | Ref ctype -> "(reference " ^ coq_type ctype ^ ")"
  | Struct na -> string_of_name na ^ ".t"
  | Function na -> string_of_name na ^ ".body"

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
  | GetElementPtr -> assert false
  | Ret -> assert false
  | ICmp -> true
  | FCmp -> true
  | PHI -> true
  | Select -> true
  | _ -> false


let rec parse_type llty =
  match Llvm.classify_type llty with
  | Llvm.TypeKind.Void -> Void
  | Llvm.TypeKind.Integer -> Int
  | Llvm.TypeKind.Pointer -> Ref (parse_type (Llvm.element_type llty))
  | Llvm.TypeKind.Array -> Ref (parse_type (Llvm.element_type llty))
  | Llvm.TypeKind.Struct -> Struct (mkName (Llvm.struct_name llty))
  | Llvm.TypeKind.Function -> Function (mkName (Some "func"))
  | _ -> raise (UnsupportType llty)

let emit_func_head lv =
  (* name should be used for file name *)
  (* let func_name = Llvm.value_name lv; *)
  Printf.printf "Definition\n";
  let func_ty = Llvm.element_type @@ Llvm.type_of lv in
  let ptypes = Llvm.param_types func_ty in
  let rettyp = Llvm.return_type func_ty in
  Printf.printf "%s :\n  %s :=\n"
    (Array.fold_left (fun acc llty ->
      let typ = parse_type llty in
      let coqtyp = coq_type typ in
      acc ^ " " ^ coqtyp
    ) "body" ptypes)
    (coq_type (parse_type rettyp))

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

let emit_llvm_inst latice lli =
  try
    let operands = get_operands lli in
    let opcode = get_opcode lli in
    if Llvm.is_terminator lli then begin
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
