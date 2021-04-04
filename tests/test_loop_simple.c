int foo(int x) {
  for (int i=0;i<10;i++) {
    x += x + 1;
  }
  return x;
}

int r(int x) {
  return x + 1;
}
