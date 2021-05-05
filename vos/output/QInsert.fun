
#include "func_head.template"

Definition
body (qhead:(reference T_q_head)) (qnodeprev:(reference T_q_node)) (qnode:(reference T_q_node)): monad (state CType) Z :=
  (* context: tt *)
  __context %= match basic_block with
    | exception =>
      qhead %= alloca (i32 1);
      qnodeprev %= alloca (i32 1);
      qnode %= alloca (i32 1);
      node %= alloca (i32 1);
      set_obj qhead qhead;;
      (* llvm debug info detected *)
      set_obj qnodeprev qnodeprev;;
      (* llvm debug info detected *)
      set_obj qnode qnode;;
      (* llvm debug info detected *)
      (* llvm debug info detected *)
      ___ssa8 %= get_obj qnodeprev;
      cmp %= ret (icomp ___ssa8 null);
      (* context: tt *)
      __context %= match cmp with
        | true =>
          ___ssa17 %= get_obj qnodeprev;
          Next %= ret (q_node.Field.Next ___ssa17);
          ___ssa18 %= get_obj Next;
          set_obj ___ssa18 node;;
          ___ssa20 %= get_obj qnode;
          ___ssa21 %= get_obj qnodeprev;
          Next2 %= ret (q_node.Field.Next ___ssa21);
          set_obj ___ssa20 Next2;;
          ret tt
          (* fallthrough *)
        | false =>
          ___ssa10 %= get_obj qhead;
          Head %= ret (q_head.Field.Head ___ssa10);
          ___ssa11 %= get_obj Head;
          set_obj ___ssa11 node;;
          ___ssa13 %= get_obj qnode;
          ___ssa14 %= get_obj qhead;
          Head1 %= ret (q_head.Field.Head ___ssa14);
          set_obj ___ssa13 Head1;;
          ret tt
          (* fallthrough *)
      end;
      let tt := __context in
      (* context: tt *)
      __context %= match basic_block with
        | exception =>
          ___ssa24 %= get_obj node;
          cmp3 %= ret (icomp ___ssa24 null);
          (* context: tt *)
          __context %= match cmp3 with
            | true =>
              ___ssa30 %= get_obj qnode;
              ___ssa31 %= get_obj node;
              Prev %= ret (q_node.Field.Prev ___ssa31);
              set_obj ___ssa30 Prev;;
              ret tt
              (* fallthrough *)
            | false =>
              ___ssa26 %= get_obj qnode;
              ___ssa27 %= get_obj qhead;
              Tail %= ret (q_head.Field.Tail ___ssa27);
              set_obj ___ssa26 Tail;;
              ret tt
              (* fallthrough *)
          end;
          let tt := __context in
          ___ssa34 %= get_obj node;
          ___ssa35 %= get_obj qnode;
          Next7 %= ret (q_node.Field.Next ___ssa35);
          set_obj ___ssa34 Next7;;
          ___ssa37 %= get_obj qnodeprev;
          ___ssa38 %= get_obj qnode;
          Prev8 %= ret (q_node.Field.Prev ___ssa38);
          set_obj ___ssa37 Prev8;;
          ret (i32 0)
        | exception =>
          raise 0
      end;
      let tt := __context in
    | exception =>
      raise 0
  end;
  let tt := __context in
  .
