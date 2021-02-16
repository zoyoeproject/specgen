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
  {type_ind: Type}
  {ind: type_ind}
  {otype:Type}
  {ti: type_of_ind ind otype}
  (ref:reference ind)
  Q :
  { fun s => Q (dereference ref s) s }
    [| get_obj ref |]
  { Q }.
Proof.
  unfold hoare_triple, bind, get_obj in *.
  firstorder.
Qed.

Lemma
  set_obj_wp
  {type_ind: Type}
  {ind: type_ind}
  {otype: Type}
  {ti: type_of_ind ind otype}
  (o: otype)
  (ref: reference ind)
  Q :
  { fun s => Q tt (set_memory s (set ref o)) }
    [| set_obj o ref |]
  { Q }.
Proof.
  unfold hoare_triple, bind, set_obj in *.
  firstorder.
Qed.


