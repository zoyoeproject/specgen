open Lltrans.Utils

let emit_llfun_body emitter llfun =
  let module ValueMap = Map.Make(String) in
  let module LlvalueMap = Map.Make(struct
    type t = Llvm.llvalue
    let compare a b = String.compare (Llvm.value_name a) (Llvm.value_name b)
    end
  ) in
  let bbmap, _ = Llvm.fold_left_blocks (fun (map,i) llb ->
    let llv = Llvm.value_of_block llb in
    let llname = Llvm.value_name llv in
    ValueMap.add llname i map, i + 1
  ) (ValueMap.empty, 0) llfun in

  let phi_lattice = Llvm.fold_left_blocks (fun m bb->
    Llvm.fold_left_instrs (fun acc v ->
       update_phi_lattice (fun lattice src lli ->
         LlvalueMap.add src lli lattice
       ) acc v
    ) m bb
  ) LlvalueMap.empty llfun in

  let var_lattice lli =
    let typ = Llvm.type_of lli in
    Lltrans.TypeBuilder.record_type typ;
    match LlvalueMap.find_opt lli phi_lattice with
    | Some _ (* l *) -> lli
    | _ -> lli
  in

  let module BB = BasicBlock (struct
    let get_block_id na = ValueMap.find na bbmap
  end) in

  let module CFG = Codeflow.Cfg.Make (LlvmStatement) (BB) in

  let translator llblock =
    let llvalue = Llvm.value_of_block llblock in
    let statement = Llvm.fold_left_instrs (fun acc v ->
      let stmt = Lltrans.FuncBuilder.emit_llvm_inst var_lattice v in
      CFG.Statement.bind [] acc stmt
    ) (CFG.Statement.mkFallThrough ()) llblock in
    let llterminator = Llvm.block_terminator llblock in
    let exists = Lltrans.FuncBuilder.translate_exits (Option.get llterminator) in
    (llvalue, statement), exists
  in

  let bset = Llvm.fold_left_blocks (fun bset llb ->
    CFG.BlockSet.add llb bset
  ) CFG.BlockSet.empty llfun in

  let entry_block = Llvm.entry_block llfun in
  let aggregate = CFG.aggregate entry_block bset false in
  let st = CFG.trace aggregate (CFG.Statement.mkFallThrough ())
    aggregate None (Llvm.value_of_block entry_block, entry_block)
    translator in
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
