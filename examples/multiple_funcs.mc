/*
 * Example with multiple functions
 * Should print 2
 */
int notok() {
  return 2;
}

void ok() {
}

int main(int argc, char**argv) {
  int a;
  ok();
  a = notok();
  print(a);
  return 0;
}

