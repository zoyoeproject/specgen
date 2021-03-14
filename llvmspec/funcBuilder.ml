open Lltrans.Utils

let phi_lattice = Hashtbl.create 20
let phi_table = Hashtbl.create 20

let emit_llfun_body emitter llfun =
  let repr_table = Hashtbl.create 20 in
  let temp_counter = ref 0 in

  let add_repr lli =
    match Llvm.classify_value lli with
    | Llvm.ValueKind.Instruction _ -> begin
        let name = Llvm.value_name lli in
        let name = if name = "" then begin
          temp_counter := !temp_counter + 1;
          "___ssa" ^ (string_of_int !temp_counter)
        end else
          List.hd (String.split_on_char '.' name)
        in Hashtbl.replace repr_table lli name
      end
    | _ -> ()
  in

  let module LlvmValue = struct
    type t = Llvm.llvalue
    type code = (* Llvm.Opcode.t *) string
    let code_to_string c = c
    let to_string a = value_to_string (fun c -> Hashtbl.find repr_table c) a
  end in

  let module ValueMap = Map.Make(String) in

  let bbmap, _ = Llvm.fold_left_blocks (fun (map,i) llb ->
    let llv = Llvm.value_of_block llb in
    let llname = Llvm.value_name llv in
    ValueMap.add llname i map, i + 1
  ) (ValueMap.empty, 0) llfun in

  let _ = Llvm.fold_left_blocks (fun _ bb->
    Llvm.fold_left_instrs (fun () v ->
      add_repr v;
      update_phi_lattice (fun b src lli ->
        if (is_phi_value src) then begin
          Hashtbl.add phi_lattice src lli
        end else if (is_constant src) then begin
          let phi_claims = match Hashtbl.find_opt phi_table b with
            | None -> Hashtbl.add phi_table b []; []
            | Some ls -> ls
          in
          Hashtbl.replace phi_table b ((lli,src) :: phi_claims)
        end else ()
      ) v
    ) () bb
  ) () llfun in

  let var_lattice lli =
    let typ = Llvm.type_of lli in
    Lltrans.TypeBuilder.record_type typ;
    match Hashtbl.find_opt phi_lattice lli with
    | Some l -> l
    | _ -> lli
  in

  let module Translator = Lltrans.BlockBuilder.Translator (LlvmValue) in
  let module BB = BasicBlock (struct
    let get_block_id na = ValueMap.find na bbmap
  end) (Translator.LlvmStatement.Exp) in

  let module CFG = Codeflow.Cfg.Make (Translator.LlvmStatement) (BB) in

  let bset = Llvm.fold_left_blocks (fun bset llb ->
    CFG.BlockSet.add llb bset
  ) CFG.BlockSet.empty llfun in

  let entry_block = Llvm.entry_block llfun in
  let aggregate = CFG.aggregate entry_block bset false in
  let st = CFG.trace aggregate (CFG.Statement.mkFallThrough ())
    aggregate None (Llvm.value_of_block entry_block, entry_block)
    (Translator.translator var_lattice phi_table)
  in
  CFG.Statement.emit emitter (snd st)

(*
@@ Codeflow.Emitter.mkEmitter ()
 *)

let emit_llfun emitter llfun =
  Codeflow.Emitter.emitLine emitter "#include \"func_head.template\"";
  Codeflow.Emitter.emitEmptyLine emitter;
  Lltrans.BlockBuilder.emit_func_head emitter llfun;
  let emitter = Codeflow.Emitter.indent emitter in
  emit_llfun_body emitter llfun;
  Codeflow.Emitter.emitLine emitter ".";
  Codeflow.Emitter.emitEmptyLine emitter;
  ()
