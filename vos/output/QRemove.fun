
#include "func_head.template"

Definition
body (qhead:(reference T_q_head)) (qnode:(reference T_q_node)): monad (state CType) Z :=
  (* context: tt *)
  __context %= match basic_block with
    | exception =>
      qhead %= alloca (i32 1);
      qnode %= alloca (i32 1);
      set_obj qhead qhead;;
      (* llvm debug info detected *)
      set_obj qnode qnode;;
      (* llvm debug info detected *)
      ___ssa5 %= get_obj qnode;
      Prev %= ret (q_node.Field.Prev ___ssa5);
      ___ssa6 %= get_obj Prev;
      cmp %= ret (icomp ___ssa6 null);
      (* context: tt *)
      __context %= match cmp with
        | true =>
          ___ssa10 %= get_obj qnode;
          Next %= ret (q_node.Field.Next ___ssa10);
          ___ssa11 %= get_obj Next;
          cmp1 %= ret (icomp ___ssa11 null);
          (* context: tt *)
          __context %= match cmp1 with
            | true =>
              ___ssa15 %= get_obj qnode;
              Next5 %= ret (q_node.Field.Next ___ssa15);
              ___ssa16 %= get_obj Next5;
              ___ssa17 %= get_obj qnode;
              Prev6 %= ret (q_node.Field.Prev ___ssa17);
              ___ssa18 %= get_obj Prev6;
              Next7 %= ret (q_node.Field.Next ___ssa18);
              set_obj ___ssa16 Next7;;
              ___ssa20 %= get_obj qnode;
              Prev8 %= ret (q_node.Field.Prev ___ssa20);
              ___ssa21 %= get_obj Prev8;
              ___ssa22 %= get_obj qnode;
              Next9 %= ret (q_node.Field.Next ___ssa22);
              ___ssa23 %= get_obj Next9;
              Prev10 %= ret (q_node.Field.Prev ___ssa23);
              set_obj ___ssa21 Prev10;;
              ret tt
              (* fallthrough *)
            | false =>
              ___ssa13 %= get_obj qhead;
              (* llvm debug info detected *)
              ret tt
              (* fallthrough *)
          end;
          let tt := __context in
        | false =>
          ___ssa8 %= get_obj qhead;
          (* llvm debug info detected *)
          ret tt
          (* fallthrough *)
      end;
      let tt := __context in
      ret (i32 0)
    | exception =>
      raise 0
  end;
  let tt := __context in
  .
