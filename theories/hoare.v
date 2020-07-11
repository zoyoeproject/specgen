Require Import Core.monadic.

(*
 * The standardard hoare logic
 * for backward reasoning
 *)
Lemma
  hoare_backward_reason
  {R:Type} {R':Type} {S:Type}
  (f: @monad S R)
  (g: R-> @monad S R')
  P P' Q
  (back: forall r, { P' r } [| g r |] { Q })
  (pre: { P } [| f |] { P' })
  : { P } [| f >>= g |] { Q }.
Proof.
  unfold bind, hoare_triple in *.
  auto.
Qed.

Lemma
  hoare_conj
  {R:Type} {S:Type}
  (f: @monad S R)
  P P' Q Q'
  (left: { P } [| f |] { Q })
  (right: { P' } [| f |] { Q' })
  : {fun s => P s /\ P' s } [| f |] {fun r s => Q r s /\ Q' r s }.
Proof.
  unfold bind, hoare_triple in *.
  firstorder.
Qed.

(* To strengthen the pre-condition *)
Lemma
  hoare_pre
  {R:Type} {S:Type}
  (P P': S -> Prop) Q
  (f: @monad S R)
  (pre: forall s, P' s -> P s)
  (hoare: { P } [| f |] { Q })
  : { P' } [| f |] { Q }.
Proof.
  unfold bind, hoare_triple in *.
  firstorder.
Qed.  

(* If prod does not dependents on the state
 * and the return value, it is safe to lift it
 * to the pre-condition
 *)
Lemma
  hoare_forall_lift
  {R S: Type}
  (P P': S -> Prop) Q
  Prod
  (f: @monad S R)
  (hoare: { P } [| f |] { Q })
  : {fun s => Prod -> P s} [| f |] {fun r s => Prod -> Q r s}.
Proof.
  unfold bind, hoare_triple in *.
  firstorder.
Qed.

