
#include "type.template"

Inductive CType :=
  | T_q_node
  | Int
  | T_q_head
  | Ref: CType -> CType
.
Module q_head.
  Record t := {
    Head: (reference T_q_node);
    Tail: (reference T_q_node)
  }.
  Module Field.
    Definition Head ptr: reference (Ref T_q_node) := ptr.
    Definition Tail ptr: reference (Ref T_q_node) := ptr.
  End Field.
  Instance ti: type_of_ind T_q_head t := {}.
End q_head.
Module q_node.
  Record t := {
    Prev: (reference T_q_node);
    Next: (reference T_q_node);
    Node: Z;
    Key: Z
  }.
  Module Field.
    Definition Prev ptr: reference (Ref T_q_node) := ptr.
    Definition Next ptr: reference (Ref T_q_node) := ptr.
    Definition Node ptr: reference (Int) := ptr.
    Definition Key ptr: reference (Int) := ptr.
  End Field.
  Instance ti: type_of_ind T_q_node t := {}.
End q_node.

(* Bind Int type to Z *)
Instance z_ti: type_of_ind Int Z := {}.
