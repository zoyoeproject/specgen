clang -fno-discard-value-names -c -emit-llvm -O1 test.c
dune build specBuilder.exe
./_build/default/specBuilder.exe test.bc
