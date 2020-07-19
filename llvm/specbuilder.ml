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

exception UnsupportType of Llvm.lltype

let rec parse_type llty =
  match Llvm.classify_type llty with
  | Llvm.TypeKind.Integer  -> Int
  | Llvm.TypeKind.Pointer  -> Ref (parse_type (Llvm.element_type llty))
  | Llvm.TypeKind.Array -> Ref (parse_type (Llvm.element_type llty))
  | Llvm.TypeKind.Struct -> Struct (mkName (Llvm.struct_name llty))
  | _  -> raise (UnsupportType llty)

let emit_func_head lv =
  (* name should be used for file name *)
  (* let func_name = Llvm.value_name lv; *)
  Printf.printf "Definition\n";
  let func_ty = Llvm.type_of lv in
  let ptypes = Llvm.param_types func_ty in
  let rettyp = Llvm.return_type func_ty in
  Printf.printf "%s : %s :=\n"
    (Array.fold_left (fun acc llty ->
      let typ = parse_type llty in
      let coqtyp = coq_type typ in
      acc ^ " " ^ coqtyp
    ) "body " ptypes)
    (coq_type (parse_type rettyp))

let emit_func_body lv =
  Llvm.iter_blocks
    (fun llbb ->
      Printf.printf "  bb: %s\n" (Llvm.value_name (Llvm.value_of_block (llbb))) ;
      Llvm.iter_instrs
        (fun lli ->
          Printf.printf "    instr: %s\n" (Llvm.string_of_llvalue lli)
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
