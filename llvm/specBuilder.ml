let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in

  Llvm.dump_module llm;
  (* Uncomment this for debugging purpose
   * Llvm.dump_module llm;
   *)

  Llvm.iter_functions CfgBuilder.emit_llfun llm;

  let emitter = Codeflow.Emitter.mkEmitter () in
  Lltrans.TypeBuilder.emit_types emitter;

  (*
  Printf.printf "*** iter_globals ***\n";
  Llvm.iter_globals print_val llm;
  *)

  ()
