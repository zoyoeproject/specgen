open Utils
val emit_llvm_inst: (Llvm.llvalue -> Llvm.llvalue) -> Llvm.llvalue -> LlvmStatement.t
val translate_exits: Llvm.llvalue -> (Llvm.llvalue * Llvm.llbasicblock) list
val emit_func_head: Llvm.llvalue -> unit

