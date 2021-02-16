open Core
let do_compile src_file =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file src_file in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in

  (*
   * Uncomment this for debugging purpose
   * Llvm.dump_module llm;
   *)

  Llvm.iter_functions FuncBuilder.emit_llfun llm;

  let emitter = Codeflow.Emitter.mkEmitter () in
  ignore @@ Lltrans.TypeBuilder.emit_types emitter

  (*
   * Printf.printf "*** iter_globals ***\n";
   * Llvm.iter_globals print_val llm;
   *)

let command =
  Command.basic
    ~summary:"Translate .bc files into a coq specification"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
     map (anon ("filename" %: string))
       ~f:(fun filename -> (fun () -> do_compile filename)))

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command


