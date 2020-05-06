/*
 * Multiple functions that should typecheck fine
 */

int iok() {
  return 1+1;
}

char cok() {
  return 'a';
}

void ok() {
}

void vvok() {
  return;
}

int main() {
  int a;
  char b;
  ok();
  a = iok();
  b = cok();
  return 0;
}
