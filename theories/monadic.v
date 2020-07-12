Require Import Bool.

(*
 * Shadow Embeded definition for code flow graph
 * using standard state monad to encode side effect of
 * monadic functions
 *)

Definition monad (S:Type) (R:Type) := S -> (R * S).

(* The target of the following notation is to mimic
 * a C-99 similar syntax:
 *
 * code_block:
 *      v1 %= f;
 *      v2 %= g v1;
 *      v3 %= h v1 v2;
 *      # v3; (# for return, coq uses return as keyword)
 *)

Definition
  bind
  {S R R':Type}
  (f: @monad S R)
  (g: R -> @monad S R')
  : (@monad S R')
  := fun (s:S) => g (fst (f s)) (snd (f s)).

(*
 * Since coq reserved "=" for its own equation we using "?=" instead.
 *)

Notation "f >>= g" := (bind f g) (at level 80).

Notation "x %= f ; g" := (f >>= (fun x => g )) (at level 80, right associativity).

Notation "f ;; g" := (f >>= (fun tt => g)) (at level 80, right associativity).

(*
 * With the above notation we will be able to define sequencial code blocks
 * using assignment with side effect
 *)

(* Basic ret function which lift a RHS into a monadic function *) Definition x:= 1.
Definition
  ret {S R:Type}
  (r:R)
  : @monad S R
  := fun s => (r, s)
.

Notation "# r" := (ret r) (at level 75).

(* To support try and catch we wrap the return value with an exception maybe monad *)

Inductive maybe_exception {E:Type} {R:Type}
  :=
  | _R : R -> @maybe_exception E R
  | _E : E -> @maybe_exception E R.

(* The target of using an exception monad is to support limited try ... catch ... syntax
 * as following:
 * try
 *   code_block (monadic notations with type @monad S (@maybe_exception R E))
 * catch e:
 *   code_block (monadic notations with type @monad S R)
 *
 * With the above setting, the whole block of try ... catch .. is of monad S R while in
 * the try block, maybe_exception R E is used so that we can encode raise in try block.
 *)

Definition
  liftE (S R E:Type)
  (m: monad S R)
  : (monad S (@maybe_exception E R))
  := fun (s:S) => (_R (fst (m s)), (snd (m s))) .

Definition
  try_catch {S E R:Type}
  (f: @monad S (@maybe_exception E R))
  (g: E -> @monad S R)
  : monad S R
  := fun s =>
       let (r, s') := f s in
       match r with
       | _R r => (r, s')
       | _E e => g e s'
       end
.

Definition
  ebind {S E R: Type}
  (f: @monad S (@maybe_exception E R))
  (g: R -> @monad S (@maybe_exception E R))
  :=
    fun s =>
      let (r, s) := f s in
      match r with
      | _R r => g r s
      | _E e => (r, s)
      end
.

Definition
  retE {S R E: Type}
  (r:R)
  : @monad S (@maybe_exception E R)
  := fun s => ((_R r), s)
.

Definition
  raise {S R E: Type}
  (e:E)
  : @monad S (@maybe_exception E R)
  := fun s => ((_E e), s)
.

(* Standard hoare triple setting on monadic functions with shadow embeded return value *)

Definition
  hoare_triple {S R: Type}
  (f: monad S R)
  (P: S -> Prop)
  (Q: R -> S -> Prop)
  : Prop
  := forall s, P s -> Q (fst (f s)) (snd (f s))
.

Notation "{ P } [| f |] { Q }" := (hoare_triple f P Q) (no associativity).

Lemma
  hoare_split
  {S R R' E:Type}
  {P: S -> Prop}
  {Q: R -> S -> Prop}
  {Q': R' -> S -> Prop}
  (f : @monad S R)
  (g : R -> @monad S R')
  (hoare_g: forall x, { Q x } [| g x |] { Q' })
  (hoare_f: { P } [| f |] { Q })
  : { P } [|
       x %= f;
       g x
    |] { Q'}.
Proof.
  unfold hoare_triple, bind in *.
  auto.
Qed.

Lemma
  hoare_if
  {S R R':Type}
  {P: S -> Prop}
  {P': S -> Prop}
  {Q: R -> S -> Prop}
  {Q': R' -> S -> Prop}
  (c : bool)
  (f : @monad S R)
  (g : @monad S R)
  (hoare_g: { P' } [| g |] { Q })
  (hoare_f: { P } [| f |] { Q })
  : { fun s =>
        (Is_true c -> P s)
     /\ (~ Is_true c -> P' s) } [|
      if c then f else g
    |] { Q }.
Proof.
  unfold hoare_triple, bind, Is_true in *.
  destruct c; intros; firstorder.
Qed.
