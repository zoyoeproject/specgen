open Utils

val emit_llvm_inst: (Llvm.llvalue -> Llvm.llvalue) -> Llvm.llvalue -> LlvmStatement.t
val translate_exits: Llvm.llvalue -> (string * Llvm.llvalue * Llvm.llbasicblock) list
val emit_func_head: Llvm.llvalue -> unit
val is_debug_fun_decl: Llvm.llvalue -> bool
