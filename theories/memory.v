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
  mem                 (* the updated abstract memory *)
  : state type_ind
  := STATE Forward type_ind mem.

(* Get the content of ref from the state *)
Definition
  dereference
  {Forward: Type} {type_ind: type_of_ind Forward}
  (ref: reference) (ft:Forward)
  (s: state type_ind)
  := (mem type_ind s) ref ft
.

Definition
  get_obj
  {Forward: Type} {type_ind: type_of_ind Forward}
  (ref: reference)
  (ft:Forward)
  := (fun s => (dereference ref ft s, s)).

(* Set the content of abstract_memory which corresponding to
 * { *ref = o; } for state s
 *)
Definition
  set
  {Forward: Type} {type_ind: type_of_ind Forward}
  (s: state type_ind ) (ref: reference) {ft: Forward}
  (o: (@to Forward) type_ind ft) : abstract_memory type_ind
.
  unfold abstract_memory.
  intros.
  refine (if (@beq Forward type_ind ft ft0) then _  else _).
Admitted.


(* In C99, *x can be LHS of assign, to minic this, we might need a
 * similar notation in coq later.
 * Currently we cook the definition of set_obj first
 *)
Definition
  set_obj {Forward:Type} {type_ind: type_of_ind Forward} {ft: Forward}
  (o: (@to Forward) type_ind ft)
  ref
  := (fun (s: state type_ind) => (tt, set_memory s (set s ref o))).

(* Test the correctiness of notation set_obj and get_obj *)
Definition
  setter_getter_test {Forward:Type} {type_ind: type_of_ind Forward}
  ref
  (t:Forward)
  :=
    o %= get_obj ref t;
    set_obj o ref
.

(* Introduce an type class of abstract spec since the spec is defined case
 * by case after C files are translated
 *)
