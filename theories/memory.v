Require Import Core.monadic.
Require Import ZArith.
Require Import Bool.

(* Linear address space with no boundary check *)
Local Open Scope Z_scope.

(* Memory model with refinemnt mapping *)
Definition reference {type_ind: Type} (ind:type_ind) := Z.

(* Handle C forward type declaration by a type class *)
Class type_of_ind {type_ind: Type} (ind: type_ind) (otype:Type) := {}.

(*
 * Introduce an type class of abstract spec since the spec is defined case
 * by case after C files are translated.
 *
 * Since the forward declaration of all c types is declared later after c files are
 * translated, we define abstract_memory through an instance of type_of_ind which
 * will be provided after c types are translated.
 *)

Definition
  abstract_memory
  (type_ind:Type)
  := forall (ind: type_ind) (otype: Type)
            (ti: type_of_ind ind otype)
            (r:reference ind), otype.

Record state (type_ind:Type) :=
  STATE
    {
      mem : (abstract_memory type_ind)
    }
.

(* Set the abstract_memory of state s *)
Definition
  set_memory
  {type_ind: Type}
  (s: state type_ind) (* the state *)
  mem_func            (* the updated abstract memory *)
  : state type_ind
  := STATE type_ind (mem_func (mem type_ind s)).

(*
 * Deference: Get the content of ref from the state
 * A type paremeter ft is provided since we need
 * to know the type of the object stored at
 * reference ref.
 *)
Definition
  dereference
  {type_ind: Type}
  {ind: type_ind}
  {otype: Type}
  {ti: type_of_ind ind otype}
  (ref: reference ind)
  (s: state type_ind)
  := (mem type_ind s) ind otype ti ref
.

(*
 * Set the content of abstract_memory which corresponding to
 * ( *ref = o; )
 * for state s.
 *)
Parameter
  set: forall
    {type_ind: Type}
    {ind: type_ind}
    {otype: Type}
    {ti: type_of_ind ind otype}
    (ref: reference ind)
    (o: otype)
    (s: abstract_memory type_ind )
  , abstract_memory type_ind
.

(* The monadic version of deference *)
Definition
  get_obj
  {type_ind: Type}
  {ind: type_ind}
  {otype: Type}
  {ti: type_of_ind ind otype}
  (ref: reference ind)
  := (fun s => (dereference ref s, s)).


(*
 * Monadic version of set:
 *
 * In C99, *x can be LHS of assign, to minic this, we might need a
 * similar notation in coq later.
 * Currently we cook the definition of set_obj as follows.
 *)
Definition
  set_obj
  {type_ind:Type}
  {ind: type_ind}
  {otype: Type}
  {ti: type_of_ind ind otype}
  (o: otype)
  ref
  := (fun (s: state type_ind) => (tt, set_memory s (set ref o))).

(* Test the correctiness of notation set_obj and get_obj *)
Definition
  setter_getter_test
  {type_ind:Type}
  {ind: type_ind}
  {otype: Type}
  {ti: type_of_ind ind otype}
  (ref: reference ind)
  :=
    o %= get_obj ref;
    set_obj o ref
.


