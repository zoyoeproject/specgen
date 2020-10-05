struct A {
  int x;
  short y;
  short z;
  struct A* ptr;
};

int foo (struct A* y) {
  return y->z;
}
