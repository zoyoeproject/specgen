#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "llvm-c/Core.h"
#include "llvm-c/Support.h"
#include "llvm/Config/llvm-config.h"
//#include "llvm/IR/Function.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"

/* llvalue -> string */
extern "C" CAMLprim value llvm_call_info(LLVMValueRef Val) {
 return caml_copy_string(LLVMGetValueName(Val));
 /*
  Function *fun = (CallInst *)call->getCalledFunction();
  return caml_copy_string(fun->getName()); // inherited from llvm::Value
  */
}
