
(*
let dump llm =
  Printf.printf "*** lookup_function ***\n" ;
  let opt_lv = Llvm.lookup_function "main" llm in
  begin
  match opt_lv with
  | Some lv -> print_val lv
  | None    -> Printf.printf "'main' function not found\n"
  end ;

  let count =
    Llvm.fold_left_functions
      (fun acc lv ->
        print_val lv ;
        acc + 1
      )
      0
      llm
  in
  Printf.printf "Functions count: %d\n" count ;


*)
