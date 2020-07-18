type name =
  | Anonymous
  | Name of string

let string_of_name na = match na with
  | Anonymous -> false
  | Name str -> str

type c_type =
  | Void
  | Int
  | Ptr of c_type
  | Struct of name * (name * c_type array)
  | Function of name * (name * c_type array)

let string_of_ctype ctype name_only =
  match ctype with
  | Void -> "unit"
  | Int -> "Z"
  | Ptr ctype -> "ref " ^ string_of_ctype ctype true
  | Struct (na, fields) -> string_of_name na
  | Function _ -> "function"

exception UnsupportType of string

let parse_type llty name_only =
  match llty with
  | Llvm.TypeKind.Integer  -> Int
  | Llvm.TypeKind.Pointer  -> mkPtr (parse_type (Llvm.element_type llty) true)
  | Llvm.TypeKind.Array -> mkPtr (parse_type (Llvm.element_type llty) true)
  (* | Llvm.TypeKind.Struct -> mkStruct *)
  | _  -> raise UnsupportType llty

let print_func_def func =
  Printf.printf "Definition\n";
  Printf.printf "  name %s\n" (Llvm.value_name lv);
  let func_ty = Llvm.type_of lv in

let print_val lv =
  Printf.printf "Value\n" ;
  Printf.printf "  name %s\n" (Llvm.value_name lv) ;
  let llty = Llvm.type_of lv in
  Printf.printf "  type %s\n" (Llvm.string_of_lltype llty) ;
  print_type llty ;
  ()

let print_fun lv =
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

let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  (*Llvm.dump_module llm ;*)

  Printf.printf "*** iter_functions ***\n" ;
  Llvm.iter_functions print_val llm ;

  Printf.printf "*** basic blocks/instructions ***\n" ;
  Llvm.iter_functions print_fun llm ;

  Printf.printf "*** iter_globals ***\n" ;
  Llvm.iter_globals print_val llm ;

  ()
