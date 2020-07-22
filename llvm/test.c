struct A {
  int x;
};
int foo (struct A *a, int y) {
  int x = 5 + y;
  return x + 1;
}
