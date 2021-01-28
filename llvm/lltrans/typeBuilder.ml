open Utils

exception DestTypeError

type type_name =
  | Void
  | Int
  | Ref of type_name
  | Struct of name
  | Function of name

let dest_struct_name = function
  | Struct n -> n
  | _ -> raise DestTypeError

let is_struct_type = function
  | Struct _ -> true
  | _ -> false

let coq_type_module tname =
  match tname with
  | Struct na -> string_of_name na
  | _ -> assert false

let rec coq_type tname =
  match tname with
  | Void -> "void"
  | Int -> "Z"
  | Ref ctype -> "(reference " ^ indicator_of ctype ^ ")"
  | Struct na -> string_of_name na ^ ".t"
  | Function na -> string_of_name na ^ ".body"

and indicator_of tname =
  match tname with
  | Void -> "Unit"
  | Int -> "Int"
  | Ref ctype -> "Ref " ^ indicator_of ctype
  | Struct na -> "T_" ^ string_of_name na
  | Function na -> "F_" ^ string_of_name na

let raw_struct_name s =
  Option.map (fun s ->
    let r = String.split_on_char '.' s in
    match r with
    | "struct" :: [n] -> n
    | n :: ["struct"] -> n
    | _ -> Printf.printf "%s:%s" s (List.hd r) ; assert false
  ) s


let extract_struct_name s =
  mkName @@ raw_struct_name s

let rec lltype_to_ctype llty =
  match Llvm.classify_type llty with
  | Llvm.TypeKind.Void -> Void
  | Llvm.TypeKind.Integer -> Int
  | Llvm.TypeKind.Pointer -> Ref (lltype_to_ctype (Llvm.element_type llty))
  | Llvm.TypeKind.Array -> Ref (lltype_to_ctype (Llvm.element_type llty))
  | Llvm.TypeKind.Struct -> Struct (extract_struct_name (Llvm.struct_name llty))
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

let rec record_type lltyp =
  match Llvm.classify_type lltyp with
  | Llvm.TypeKind.Pointer -> record_type @@ Llvm.element_type lltyp
  | Llvm.TypeKind.Array -> record_type @@ Llvm.element_type lltyp
  | Llvm.TypeKind.Struct -> begin
      Hashtbl.replace lltype_table (build_type_key lltyp) lltyp;
      Array.iter (fun t ->
        let type_name = lltype_to_ctype t in
        match type_name with
        | Struct _ -> record_type t
        | _ -> ()
      ) (Llvm.struct_element_types lltyp)
    end
  | _ -> Hashtbl.replace lltype_table (build_type_key lltyp) lltyp

let emit_type_indicator emitter =
  let open Codeflow in
  Emitter.emitLine emitter "Inductive CType :=";
  let e2 = Emitter.indent emitter in
  Hashtbl.iter (fun _ t ->
    let type_name = lltype_to_ctype t in
    Emitter.emitLine e2 "| %s" (indicator_of type_name);
  ) lltype_table;
  Emitter.emitLine emitter "."

let emit_record_type emitter lltyp =
  let open Codeflow in
  let type_name = lltype_to_ctype lltyp in
  Emitter.emitLine emitter "Module %s." (coq_type_module type_name);
  let e2 = Emitter.indent emitter in
  Emitter.emitLine e2 "Record t := {";
  let indent_emitter = Emitter.indent e2 in
  let element_types = Llvm.struct_element_types lltyp in
  let n = Array.length element_types in
  Array.iteri (fun i t ->
    let type_name = lltype_to_ctype t in
    let struct_name = Option.get (raw_struct_name (Llvm.struct_name lltyp)) in
    Emitter.emitLine indent_emitter "%s: %s%s"
      (Llvmdinfo.get_field_name struct_name i)
      (coq_type type_name)
      (if i = n-1 then "" else ";")
  ) element_types;
  Emitter.emitLine e2 "}.";
  Emitter.emitLine emitter "End."

let emit_types emitter =
  let open Codeflow in
  emit_type_indicator emitter;
  let type_list = List.of_seq @@ Hashtbl.to_seq_keys lltype_table in
  let type_list = List.sort (fun x y -> String.compare x y) type_list in
  ignore @@ List.iter (fun c ->
    let lltyp = Hashtbl.find lltype_table c in
    let type_name = lltype_to_ctype lltyp in
    match type_name with
    | Struct _ -> emit_record_type emitter lltyp
    | _ -> ()
  ) type_list;
  Emitter.emitEmptyLine emitter

