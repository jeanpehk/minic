/*
 * Simple recursion example that print's value at each call
 * Should print numbers from 5 to 1
 */
int rec(int a) {
  print(a);
  if (a > 1) {
    a = rec(a-1);
  }
  else {
    ;
  }
  return a;
}

int main() {
  rec(5);
}

