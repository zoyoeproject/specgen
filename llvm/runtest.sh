clang -fno-discard-value-names -g -c -emit-llvm -O1 $1 -o test.bc
dune build specBuilder.exe
./_build/default/specBuilder.exe test.bc
