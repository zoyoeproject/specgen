clang -fno-discard-value-names -c -emit-llvm -O1 test.c
dune build specbuilder.exe
./_build/default/specbuilder.exe test.bc
