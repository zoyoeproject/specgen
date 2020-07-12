Require Import Core.monadic.
Require Import ZArith.
Require Import Bool.

(* Linear address space with no boundary check *)
Local Open Scope Z_scope.

(* Memory model with refinemnt mapping *)
Definition reference := Z.

(* Handle C forward type declaration by a type class *)
Class  type_of_ind (T:Type) :=
  {
  to: forall (o: T), Type;
  beq: forall (o:T) (o':T), bool;
  beq_eq: forall (o:T) (o':T), Is_true (beq o o') <-> o = o';
  unique: forall x y, ~ (x = y) -> ~ (to x = to y)
  }
.

(*
 * Introduce an type class of abstract spec since the spec is defined case
 * by case after C files are translated.
 *
 * Since the forward declaration of all c types is declared later after c files are
 * translated, we define abstract_memory through an instance of type_of_ind which
 * will be provided after c types are translated.
 *)

Definition abstract_memory {Forward:Type} (type_ind:type_of_ind Forward)
  := forall (r:reference) (ft:Forward), (@to Forward) type_ind ft.

Record state {Forward:Type} (type_ind:type_of_ind Forward) :=
  STATE
    {
      mem : (abstract_memory type_ind)
    }
.

(* Set the abstract_memory of state s *)
Definition
  set_memory
  {Forward: Type} {type_ind:type_of_ind Forward}
  (s: state type_ind) (* the state *)
  mem_func            (* the updated abstract memory *)
  : state type_ind
  := STATE Forward type_ind (mem_func (mem type_ind s)).

(*
 * Deference: Get the content of ref from the state
 * A type paremeter ft is provided since we need
 * to know the type of the object stored at
 * reference ref.
 *)
Definition
  dereference
  {Forward: Type} {type_ind: type_of_ind Forward}
  (ref: reference) (ft:Forward)
  (s: state type_ind)
  := (mem type_ind s) ref ft
.

(*
 * Set the content of abstract_memory which corresponding to
 * ( *ref = o; )
 * for state s.
 *)
Axiom
  set: forall
  {Forward: Type} {type_ind: type_of_ind Forward}
  (ref: reference) {ft: Forward}
  (o: (@to Forward) type_ind ft)
  (s: abstract_memory type_ind )    
  , abstract_memory type_ind
.

(* The monadic version of deference *)
Definition
  get_obj
  {Forward: Type} {type_ind: type_of_ind Forward}
  (ref: reference)
  (ft:Forward)
  := (fun s => (dereference ref ft s, s)).


(*
 * Monadic version of set:
 *
 * In C99, *x can be LHS of assign, to minic this, we might need a
 * similar notation in coq later.
 * Currently we cook the definition of set_obj as follows.
 *)
Definition
  set_obj {Forward:Type} {type_ind: type_of_ind Forward} {ft: Forward}
  (o: (@to Forward) type_ind ft)
  ref
  := (fun (s: state type_ind) => (tt, set_memory s (set ref o))).

(* Test the correctiness of notation set_obj and get_obj *)
Definition
  setter_getter_test {Forward:Type} {type_ind: type_of_ind Forward}
  ref
  (t:Forward)
  :=
    o %= get_obj ref t;
    set_obj o ref
.


