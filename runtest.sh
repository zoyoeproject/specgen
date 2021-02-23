clang -fno-discard-value-names -g -c -emit-llvm -O1 $1 -o test.bc
./_build/default/llvm/specBuilder.exe test.bc test_proj
