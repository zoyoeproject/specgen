(*
 * Word arithment, Instead of putting size info onto the type of operand (GCC),
 * we put the type info on to the type of operator (LLVM style).
 *
 * The restriction is that we relies on llvm compiler to give the correct type info
 * of word arithments.
 *)
Require Import ZArith.
Open Scope Z.

(*
Definition word_plus (a:Z) (b:Z) (sz:nat)
Definition word_minus (a:Z) (b:Z) (sz:nat)
Definition word_mult (a:Z) (b:Z) (sz:nat)
Definition word_div (a:Z) (b:Z) (sz:nat)
Definition word_shiftl (a:Z) (b:Z) (sz:nat)
Definition word_shiftr (a:Z) (b:Z) (sz:nat)
*)

Definition i32 (x:Z) := x.

Definition icomp (a:Z) (b:Z) := Z.eqb a b.

Axiom wadd: Z -> Z-> Z.
Axiom wmul: Z -> Z-> Z.
