#include "llvm-c/Core.h"
#include "llvm-c/Support.h"
#include "llvm-c/DebugInfo.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Metadata.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"

using namespace llvm;


value llvm_string(char *message) {
  value str = caml_copy_string("Hello world");
  return str;
}

extern "C"
{

CAMLprim Metadata * llvm_metadata_from_value (LLVMValueRef llv) {
  auto *MD = dyn_cast_or_null<MetadataAsValue>(unwrap(llv));
  auto mnode = MD->getMetadata();
  return mnode;
}

CAMLprim value llvm_get_md_kind (MDNode *mdnode) {
  return Val_int(mdnode->getMetadataID());
}

CAMLprim Metadata * llvm_get_variable_type (MDNode *mdnode) {
  auto *md = cast_or_null<DIVariable>(mdnode);
  return (md->getType());
}

CAMLprim Metadata * llvm_get_base_type (MDNode *mdnode) {
  auto *md = cast_or_null<DIDerivedType>(mdnode);
  return (md->getBaseType());
}

CAMLprim value llvm_get_fields_name (MDNode *mdnode) {
  CAMLparam0();
  CAMLlocal2(nodes, name);
  auto di = cast_or_null<DICompositeType>(mdnode);
  DINodeArray eles = di->getElements();
  nodes = alloc(eles.size(), 0);
  for (unsigned i = 0; i < eles.size(); i++) {
    name = caml_copy_string((cast_or_null<DIType>(eles[i]))->getName().data());
    printf ("field metadata %d\n", (cast_or_null<DIType>(eles[i]))->getMetadataID());
    Store_field(nodes, i, name);
  }
  CAMLreturn(nodes);
}

CAMLprim value llvm_get_info_name (MDNode *mdnode) {
  CAMLparam0();
  CAMLlocal1(name);
  auto di = cast_or_null<DIType>(mdnode);
  name = caml_copy_string(di->getName().data());
  CAMLreturn(name);
}

CAMLprim void llvm_get_all_metadata (LLVMValueRef llv) {
  SmallVector<std::pair<unsigned, MDNode*>, 4> vec;
  (dyn_cast<Instruction>(unwrap(llv)))->getAllMetadata(vec);
  auto cur = vec.begin();
  for (;cur!=vec.end(); cur++) {
    printf("md_node kind is %d\n", (cur->second)->getMetadataID());
  }
  return;
}

CAMLprim value llvm_get_named_metadata (LLVMModuleRef m) {
  CAMLparam0();
  CAMLlocal2(names, str);
  int sz = 0;
  size_t len;
  LLVMNamedMDNodeRef mdref_cursor = LLVMGetFirstNamedMetadata(m);
  //unwrap(mdref)->dump();
  /* counting the number of NamedMetadata */
  for (; mdref_cursor != nullptr; sz++, mdref_cursor = LLVMGetNextNamedMetadata(mdref_cursor));
  names = alloc(sz, 0);
  mdref_cursor = LLVMGetFirstNamedMetadata(m);
  sz = 0;
  for (; mdref_cursor != nullptr; sz++, mdref_cursor = LLVMGetNextNamedMetadata(mdref_cursor)) {
    const char *name = LLVMGetNamedMetadataName(mdref_cursor, &len);
    str = caml_alloc_string(len);
    memcpy(String_val(str), name, len);
    Store_field(names, sz, str);
  }
  CAMLreturn(names);
}
}
