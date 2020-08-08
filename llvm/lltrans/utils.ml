open Codeflow
module type IndexMap = sig
  val get_block_id : string -> int
end

module LlvmValue = struct
  type t = Llvm.llvalue
  let to_string a = Llvm.value_name a
  let compare a b = String.compare (Llvm.value_name a) (Llvm.value_name b)
end

module LlvmStatement = Codeflow.Element.MakeStatement (LlvmValue)

module BasicBlock (Index: IndexMap)
  : (Cfg.Block with type elt = LlvmValue.t
    and type t = Llvm.llbasicblock) = struct
  type elt = LlvmValue.t
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

