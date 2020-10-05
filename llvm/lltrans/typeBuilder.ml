open Utils

type type_name =
  | Void
  | Int
  | Ref of type_name
  | Struct of name
  | Function of name

let coq_type_module tname =
  match tname with
  | Struct na -> string_of_name na
  | _ -> assert false

let rec coq_type tname =
  match tname with
  | Void -> "void"
  | Int -> "Z"
  | Ref ctype -> "(reference " ^ coq_type ctype ^ ")"
  | Struct na -> string_of_name na ^ ".t"
  | Function na -> string_of_name na ^ ".body"

let rec indicator_of tname =
  match tname with
  | Void -> "Unit"
  | Int -> "Int"
  | Ref ctype -> "Ref " ^ indicator_of ctype
  | Struct na -> "T_" ^ string_of_name na
  | Function na -> "F_" ^ string_of_name na

let rec lltype_to_ctype llty =
  match Llvm.classify_type llty with
  | Llvm.TypeKind.Void -> Void
  | Llvm.TypeKind.Integer -> Int
  | Llvm.TypeKind.Pointer -> Ref (lltype_to_ctype (Llvm.element_type llty))
  | Llvm.TypeKind.Array -> Ref (lltype_to_ctype (Llvm.element_type llty))
  | Llvm.TypeKind.Struct -> Struct (mkName (Llvm.struct_name llty))
  | Llvm.TypeKind.Function -> Function (mkName (Some "func"))
  | _ -> raise (UnsupportType llty)

let rec build_type_key lltyp =
  match Llvm.classify_type lltyp with
  | Llvm.TypeKind.Struct -> begin
    let ind = indicator_of @@ lltype_to_ctype lltyp in
    let ind = Array.fold_left (fun acc lltyp ->
      acc ^ build_type_key lltyp ^ ":"
    ) (ind ^ "[") (Llvm.struct_element_types lltyp)
    in ind ^ "]"
    end
  | _ -> indicator_of @@ lltype_to_ctype lltyp

let lltype_table = Hashtbl.create 20

let record_type lltyp =
  match Llvm.classify_type lltyp with
  | Llvm.TypeKind.Pointer -> ()
  | Llvm.TypeKind.Array -> ()
  | _ -> Hashtbl.replace lltype_table (build_type_key lltyp) lltyp

let emit_type_indicator emitter =
  let open Codeflow in
  Emitter.emitLine emitter "Inductive CType :=";
  let emitter = Emitter.indent emitter in
  Hashtbl.iter (fun _ t ->
    let type_name = lltype_to_ctype t in
    Emitter.emitLine emitter "| %s" (indicator_of type_name);
  ) lltype_table;
  Emitter.emitLine emitter "."

let emit_record_type emitter lltyp =
  let open Codeflow in
  let type_name = lltype_to_ctype lltyp in
  Emitter.emitLine emitter "module %s." (coq_type_module type_name);
  Emitter.emitLine emitter "Record t := {";
  let indent_emitter = Emitter.indent emitter in
  Array.iter (fun t ->
    let type_name = lltype_to_ctype t in
    Emitter.emitLine indent_emitter ":= %s;" (coq_type type_name)
  ) (Llvm.struct_element_types lltyp);
  Emitter.emitLine emitter "}."

let emit_types emitter =
  let type_list = List.of_seq @@ Hashtbl.to_seq_keys lltype_table in
  let type_list = List.sort (fun x y -> String.compare x y) type_list in
  ignore @@ List.iter (fun c ->
    let lltyp = Hashtbl.find lltype_table c in
    emit_record_type emitter lltyp
  ) type_list

