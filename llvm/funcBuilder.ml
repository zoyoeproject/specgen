open Lltrans.Utils

let phi_lattice = Hashtbl.create 20

module LlvmValue = struct
  type t = Llvm.llvalue
  type code = Llvm.Opcode.t
  let code_to_string = op_to_string
  let to_string a = value_to_string a
end

let emit_llfun_body emitter llfun =
  let module ValueMap = Map.Make(String) in
  let bbmap, _ = Llvm.fold_left_blocks (fun (map,i) llb ->
    let llv = Llvm.value_of_block llb in
    let llname = Llvm.value_name llv in
    ValueMap.add llname i map, i + 1
  ) (ValueMap.empty, 0) llfun in

  let _ = Llvm.fold_left_blocks (fun _ bb->
    Llvm.fold_left_instrs (fun () v ->
       update_phi_lattice (fun _ src lli ->
         if (is_phi_value src) then begin
           Hashtbl.add phi_lattice src lli
         end else ()
       ) v
    ) () bb
  ) () llfun in

  let var_lattice lli =
    let typ = Llvm.type_of lli in
    Lltrans.TypeBuilder.record_type typ;
    match Hashtbl.find_opt phi_lattice lli with
    | Some l -> l
    | _ -> lli
  in

  let module Translator = Lltrans.FuncBuilder.Translator (LlvmValue) in
  let module BB = BasicBlock (struct
    let get_block_id na = ValueMap.find na bbmap
  end) (Translator.LlvmStatement.Exp) in

  let module CFG = Codeflow.Cfg.Make (Translator.LlvmStatement) (BB) in

  let bset = Llvm.fold_left_blocks (fun bset llb ->
    CFG.BlockSet.add llb bset
  ) CFG.BlockSet.empty llfun in

  let entry_block = Llvm.entry_block llfun in
  let aggregate = CFG.aggregate entry_block bset false in
  let st = CFG.trace aggregate (CFG.Statement.mkFallThrough ())
    aggregate None (Llvm.value_of_block entry_block, entry_block)
    (Translator.translator var_lattice)
  in
  CFG.Statement.emit emitter (snd st)

let emit_llfun llfun =
  if (Lltrans.FuncBuilder.is_debug_fun_decl llfun) then
    ()
  else begin
    let emitter = Codeflow.Emitter.indent @@ Codeflow.Emitter.mkEmitter () in
    Lltrans.FuncBuilder.emit_func_head llfun;
    emit_llfun_body emitter llfun;
    Printf.printf "\n";
    ()
  end
