int foo(int x) {
  while (x>-2) {
    x++;
    if (x > 5) {
      return x;
    }
  }
  x = x + 1;
  return 3;
}
