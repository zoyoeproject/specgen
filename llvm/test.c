struct A {
  int x;
};
int foo (struct A *a, int y) {
  int x = y + 1;
  y = x + y;
  y = y + x;
  return y + 1;
}

int branch(int x) {
  if (x > 0) {
    int y = 3;
    y = y + x;
    x = x * y;
    return x + y;}
  else {
    x = x + 3;
    x = x + 3;
    return x;
  }
}
