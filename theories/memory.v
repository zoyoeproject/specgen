(* Memory model with refinemnt mapping *)

(* Linear address space with no boundary check *)
Require Import ZArith.

Definition address := Z.

(* Handle C forward type declaration by a type class *)
Class
  declared_type (Forward: Type) (Concrete: Type)
  {
    to_type: Concrete -> Forward
  }
.
  
(* Introduce an type class of abstract spec since the spec is defined case
 * by case after C files are translated
 *)
