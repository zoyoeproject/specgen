struct A {
  int x;
  int x1;
  short y;
  short y1;
  short z;
  struct A* ptr;
};

int foo (struct A* y) {
  if (y->x > 1) {
    return y->x1;
  } else {
    return 12;
  }
}
