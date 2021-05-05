
#include "func_head.template"

Definition
body (qhead:(reference T_q_head)): monad (state CType) (reference T_q_node) :=
  (* context: tt *)
  __context %= match basic_block with
    | exception =>
      qhead %= alloca (i32 1);
      node %= alloca (i32 1);
      set_obj qhead qhead;;
      (* llvm debug info detected *)
      (* llvm debug info detected *)
      ___ssa4 %= get_obj qhead;
      Tail %= ret (q_head.Field.Tail ___ssa4);
      ___ssa5 %= get_obj Tail;
      set_obj ___ssa5 node;;
      ___ssa7 %= get_obj node;
      cmp %= ret (icomp ___ssa7 null);
      (* context: tt *)
      __context %= match cmp with
        | true =>
          ret tt
          (* fallthrough *)
        | false =>
          ___ssa9 %= get_obj node;
          ___ssa10 %= get_obj qhead;
          Head %= ret (q_head.Field.Head ___ssa10);
          ___ssa11 %= get_obj Head;
          cmp1 %= ret (icomp ___ssa9 ___ssa11);
          (* context: tt *)
          __context %= match cmp1 with
            | true =>
              ___ssa21 %= get_obj qhead;
              Head5 %= ret (q_head.Field.Head ___ssa21);
              set_obj null Head5;;
              ___ssa23 %= get_obj qhead;
              Tail6 %= ret (q_head.Field.Tail ___ssa23);
              set_obj null Tail6;;
              ret tt
              (* fallthrough *)
            | false =>
              ___ssa13 %= get_obj node;
              Prev %= ret (q_node.Field.Prev ___ssa13);
              ___ssa14 %= get_obj Prev;
              ___ssa15 %= get_obj qhead;
              Tail3 %= ret (q_head.Field.Tail ___ssa15);
              set_obj ___ssa14 Tail3;;
              ___ssa17 %= get_obj node;
              Prev4 %= ret (q_node.Field.Prev ___ssa17);
              ___ssa18 %= get_obj Prev4;
              Next %= ret (q_node.Field.Next ___ssa18);
              set_obj null Next;;
              ret tt
              (* fallthrough *)
          end;
          let tt := __context in
      end;
      let tt := __context in
      ___ssa27 %= get_obj node;
      ret ___ssa27
    | exception =>
      raise 0
  end;
  let tt := __context in
  .
