open Core

let type_fname = "type.spec"

let type_file project_folder =
  Out_channel.create @@ project_folder ^ "/output/" ^ type_fname

let func_file project_folder func_name =
  Out_channel.create @@ project_folder ^ "/output/" ^ func_name


let do_compile src_file project_folder =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file src_file in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in

  (*
   * Uncomment this for debugging purpose
   * Llvm.dump_module llm;
   *)

  Llvm.iter_functions (fun llfun ->
    let func_name = Llvm.value_name llfun in
    let emitter = Codeflow.Emitter.mkEmitter
        (func_file project_folder (func_name ^ ".fun"))
    in
    FuncBuilder.emit_llfun emitter llfun
  ) llm ;

  let emitter = Codeflow.Emitter.mkEmitter (type_file project_folder) in
  ignore @@ Lltrans.TypeBuilder.emit_types emitter

  (*
   * Printf.printf "*** iter_globals ***\n";
   * Llvm.iter_globals print_val llm;
   *)

let build_info = "trial version 0.0.1"

let command =
  Command.basic
    ~summary:"Translate .bc files into a coq specification"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open filename = (anon ("filename" %: string))
      and g = (anon ("projectname" %: string))
      in
        (fun () -> do_compile filename g)
    )

let () =
  Command.run ~version:"1.0" ~build_info:build_info command


