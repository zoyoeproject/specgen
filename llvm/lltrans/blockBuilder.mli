(* open Utils *)
open Codeflow

type llvalue_lattice = Llvm.llvalue -> Llvm.llvalue

val emit_func_head: Llvm.llvalue -> unit
val is_debug_fun_decl: Llvm.llvalue -> bool

module Translator:
  functor (LlvmValue: Exp.Exp with
    type t = Llvm.llvalue and
    type code = string
  ) -> sig
  module LlvmStatement: Cfg.Statement with module Exp = LlvmValue
  val translator:
        llvalue_lattice
     -> (Llvm.llbasicblock, (Llvm.llvalue * Llvm.llvalue) list) Hashtbl.t
     -> Llvm.llbasicblock
     -> (Llvm.llvalue * LlvmStatement.t) * ((string * Llvm.llvalue * Llvm.llbasicblock) list)
end
