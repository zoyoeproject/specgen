exception UnsupportType of Llvm.lltype
exception UnsupportOperand of Llvm.llvalue
exception UnsupportOpcode

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

let rec indicator_of ctype =
  match ctype with
  | Void -> "Unit"
  | Int -> "Int"
  | Ref ctype -> "Ref " ^ indicator_of ctype
  | Struct na -> "T_" ^ string_of_name na
  | Function na -> "F_" ^ string_of_name na

let rec coq_type ctype =
  match ctype with
  | Void -> "void"
  | Int -> "Z"
  | Ref ctype -> "reference " ^ coq_type ctype
  | Struct na -> string_of_name na ^ ".t"
  | Function na -> string_of_name na ^ ".body"

let emit_operator op_code =
  let open Llvm.Opcode in
  match op_code with
  | Ret -> "ret"
  | Br -> assert false
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
  | Alloca -> assert false
  | Load -> assert false
  | Store -> assert false
  | GetElementPtr -> assert false
  | _ -> raise UnsupportOpcode

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
  Printf.printf "%s : %s :=\n"
    (Array.fold_left (fun acc llty ->
      let typ = parse_type llty in
      let coqtyp = coq_type typ in
      acc ^ " " ^ coqtyp
    ) "body" ptypes)
    (coq_type (parse_type rettyp))

let emit_operand _ llvalue =
  match Llvm.classify_value llvalue with
  | NullValue -> "NullValue"
  | Argument -> Llvm.value_name llvalue
  | ConstantInt -> Llvm.string_of_llvalue llvalue
  | ConstantExpr -> Llvm.string_of_llvalue llvalue
  | Instruction _ -> (Llvm.value_name llvalue)

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

let emit_llinst llvalue =
  match Llvm.classify_value llvalue with
  | Instruction opcode -> emit_operator opcode
  | _ -> assert false

let get_operands lli =
  Array.init (Llvm.num_operands lli) (fun n -> Llvm.operand lli n)

let emit_func_body lv =
  Llvm.iter_blocks
    (fun llbb ->
      Printf.printf "  bb: %s\n" (Llvm.value_name lv) ;
      Llvm.iter_instrs
        (fun lli ->
          let operands = get_operands lli in
          Printf.printf "  [%d] %s ?= %s\n" (Llvm.num_operands lli)
            (Array.fold_left (fun acc operand -> acc ^ " -" ^ emit_operand () operand)
                (emit_llinst lli) operands)
            (Llvm.string_of_llvalue lli)
        )
        llbb
    )
    lv

let emit_fun lv =
  emit_func_head lv;
  emit_func_body lv

let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in

  (* Uncomment this for debugging purpose
   * Llvm.dump_module llm;
   *)

  Printf.printf "*** iter_functions ***\n";
  Llvm.iter_functions emit_fun llm;

  (*
  Printf.printf "*** iter_globals ***\n";
  Llvm.iter_globals print_val llm;
  *)

  ()
