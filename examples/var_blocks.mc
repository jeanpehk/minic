/*
 * Example of variable scopes
 * Should print 1, 2 and 3
 */
int main() {
  int a;
  a = 1;
  print(a);
  {
    int a;
    a = 2;
    print(a);
  }
    {
      int a;
      a = 3;
      print(a);
    }
  return 0;
}
