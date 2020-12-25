type llvm_mdnode
type llvm_mdkind =
  | MDString
  | ConstantAsMetadata
  | LocalAsMetadata
  | DistinctMDOperandPlaceholder
  | MDTuple
  | DILocation
  | DIExpression
  | DIGlobalVariableExpression
  | GenericDINode
  | DISubrange
  | DIEnumerator
  | DIBasicType
  | DIDerivedType
  | DICompositeType
  | DISubroutineType
  | DIFile
  | DICompileUnit
  | DISubprogram
  | DILexicalBlock
  | DILexicalBlockFile
  | DINamespace
  | DIModule
  | DITemplateTypeParameter
  | DITemplateValueParameter
  | DIGlobalVariable
  | DILocalVariable
  | DILabel
  | DIObjCProperty
  | DIImportedEntity
  | DIMacro
  | DIMacroFile
  | DICommonBlockMetadataKind

external get_di_kind: llvm_mdnode-> llvm_mdkind = "llvm_get_md_kind"
external get_di_kind_num: llvm_mdnode -> int = "llvm_get_md_kind"
external metadata_from_value: Llvm.llvalue -> llvm_mdnode = "llvm_metadata_from_value"
external get_named_metadata: Llvm.llmodule -> string array = "llvm_get_named_metadata"
external get_all_metadata: Llvm.llvalue -> unit = "llvm_get_all_metadata"
external get_variable_type: llvm_mdnode -> llvm_mdnode = "llvm_get_variable_type"
external get_base_type: llvm_mdnode -> llvm_mdnode = "llvm_get_base_type"
external get_fields_name: llvm_mdnode -> string array = "llvm_get_fields_name"
external get_info_name: llvm_mdnode -> string = "llvm_get_info_name"

let type_field_tbl = Hashtbl.create 10

let llvalue_to_metadata llv =
  match Llvm.classify_value llv with
  | MDNode -> Some (metadata_from_value llv)
  | _ -> None

let rec register_dinfo mnode =
  match (get_di_kind mnode) with
  | DILocalVariable
  | DIGlobalVariable ->
    register_dinfo (get_variable_type mnode)
  | DIDerivedType ->
    register_dinfo (get_base_type mnode)
  | DICompositeType ->
    let fields = get_fields_name mnode in
    Hashtbl.replace type_field_tbl (get_info_name mnode) fields
  | _ -> ()

let register_metavalue operand =
  match (llvalue_to_metadata operand) with
  | Some mnode -> register_dinfo mnode
  | None -> ()

