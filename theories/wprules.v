Require Import Core.monadic.
Require Import Core.memory.

Lemma
  hoare_ret
  {S R:Type}
  {Q: R -> S -> Prop}
  (c: bool)
  (f: @monad S R)
  (r: R)
  : { fun s => Q r s } [| ret r |] { Q }.
Proof.
  unfold hoare_triple, bind in *.
  destruct c; intros; firstorder.
Qed.

Lemma
  get_obj_wp
  {Forward: Type}
  {type_ind: type_of_ind Forward}
  ft ref Q :
  { fun s => Q (dereference ref ft s) s }
    [| get_obj ref ft |]
  { Q }.
Proof.
  unfold hoare_triple, bind, get_obj in *.
  firstorder.
Qed.

Lemma
  set_obj_wp
  {Forward: Type}
  {type_ind: type_of_ind Forward}
  {ft: Forward}
  (o: (@to Forward) type_ind ft)
  ref Q :
  { fun s => Q tt (set_memory s (set ref o)) }
    [| set_obj o ref |]
  { Q }.
Proof.
  unfold hoare_triple, bind, set_obj in *.
  firstorder.
Qed.


