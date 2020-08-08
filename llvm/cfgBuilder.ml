open Lltrans.Utils

let emit_llfun_body emitter llfun =
  let module ValueMap = Map.Make(String) in
  let bbmap, _ = Llvm.fold_left_blocks (fun (map,i) llb ->
    let llv = Llvm.value_of_block llb in
    let llname = Llvm.value_name llv in
    ValueMap.add llname i map, i + 1
  ) (ValueMap.empty, 0) llfun in

  let module BB = BasicBlock (struct
    let get_block_id na = ValueMap.find na bbmap
  end) in

  let module CFG = Codeflow.Cfg.Make (LlvmStatement) (BB) in

  let translator llblock =
    let llvalue = Llvm.value_of_block llblock in
    let statement = Llvm.fold_left_instrs (fun acc v ->
      let stmt = Lltrans.FuncBuilder.emit_llvm_inst v in
      CFG.Statement.bind None acc stmt
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
  let emitter = Codeflow.Emitter.indent @@ Codeflow.Emitter.mkEmitter () in
  Lltrans.FuncBuilder.emit_func_head llfun;
  emit_llfun_body emitter llfun