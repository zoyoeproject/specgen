int foo(int x) {
  while (1) {
    x++;
    if (x > 5) {
      return x;
    }
  }
  return 3;
}
