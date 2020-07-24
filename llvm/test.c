struct A {
  int x;
};
int foo (struct A *a, int y) {
  int x = y + 1;
  y = x + y;
  y = y + x;
  return y + 1;
}
