open Lltrans
module type IndexMap = sig
  val get_block_id : string -> int
end

module BasicBlock (Index: IndexMap) = struct
  type elt = Llvm.llvalue
  type t = Llvm.llbasicblock
  let id b = Llvm.value_name (Llvm.value_of_block b)
  let index b = Index.get_block_id (id b)
  let compare a b = (index a) - (index b)
  let equal b a = (index a) = (index b)
  let elements b =
    Llvm.fold_left_instrs (fun acc lli -> acc @ [lli]) [] b
  let next b =
    let llterminator = Llvm.block_terminator b in
    Array.to_list (Llvm.successors (Option.get llterminator))
end

let emit_llfun llfun =
  let module StringMap = Map.Make(String) in
  let bbmap, _ = Llvm.fold_left_blocks (fun (map,i) llb ->
    let llv = Llvm.value_of_block llb in
    let name = Llvm.value_name llv in
    StringMap.add name i map, i + 1
  ) (StringMap.empty, 0) llfun in

  let module BB = BasicBlock (struct
    let get_block_id na = StringMap.find na bbmap
  end) in
  let module CFG = Codeflow.Cfg.Make (BB) in

  let translator llblock : string CFG.Statement.t =
    Llvm.fold_left_instrs (fun acc v ->
      let stmt = CFG.Statement.mkComment (FuncBuilder.emit_llvm_inst v) in
      CFG.Statement.bind None acc stmt
    ) (CFG.Statement.mkFallThrough ()) llblock
  in

  let bset = Llvm.fold_left_blocks (fun bset llb ->
    CFG.BlockSet.add llb bset
  ) CFG.BlockSet.empty llfun in

  let entry_block = Llvm.entry_block llfun in
  let aggregate = CFG.aggregate entry_block bset false in
  let st = CFG.trace aggregate (CFG.Statement.mkFallThrough ())
    aggregate None entry_block translator in
  let emitter = Codeflow.Emitter.mkEmitter () in
  CFG.Statement.emit emitter (snd st)
