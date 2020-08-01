open Lltrans
let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in

  (* Uncomment this for debugging purpose
   * Llvm.dump_module llm;
   *)

  Printf.printf "*** iter_functions ***\n";
  Llvm.iter_functions FuncBuilder.emit_fun llm;

  Llvm.iter_functions CfgBuilder.emit_llfun llm;
  (*
  Printf.printf "*** iter_globals ***\n";
  Llvm.iter_globals print_val llm;
  *)

  ()
