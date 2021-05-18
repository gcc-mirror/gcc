#include <cassert>

/* Test attach/detach operation with pointers and references to structs.  */

typedef struct mystruct {
  int *a;
  int b;
  int *c;
  int d;
  int *e;
} mystruct;

void str (void)
{
  int a[10], c[10], e[10];
  mystruct m = { .a = a, .c = c, .e = e };
  a[0] = 5;
  c[0] = 7;
  e[0] = 9;
  #pragma acc parallel copy(m.a[0:10], m.b, m.c[0:10], m.d, m.e[0:10])
  {
    m.a[0] = m.c[0] + m.e[0];
  }
  assert (m.a[0] == 7 + 9);
}

void strp (void)
{
  int *a = new int[10];
  int *c = new int[10];
  int *e = new int[10];
  mystruct *m = new mystruct;
  m->a = a;
  m->c = c;
  m->e = e;
  a[0] = 6;
  c[0] = 8;
  e[0] = 10;
  #pragma acc parallel copy(m->a[0:10], m->b, m->c[0:10], m->d, m->e[0:10])
  {
    m->a[0] = m->c[0] + m->e[0];
  }
  assert (m->a[0] == 8 + 10);
  delete m;
  delete[] a;
  delete[] c;
  delete[] e;
}

void strr (void)
{
  int *a = new int[10];
  int *c = new int[10];
  int *e = new int[10];
  mystruct m;
  mystruct &n = m;
  n.a = a;
  n.c = c;
  n.e = e;
  a[0] = 7;
  c[0] = 9;
  e[0] = 11;
  #pragma acc parallel copy(n.a[0:10], n.b, n.c[0:10], n.d, n.e[0:10])
  {
    n.a[0] = n.c[0] + n.e[0];
  }
  assert (n.a[0] == 9 + 11);
  delete[] a;
  delete[] c;
  delete[] e;
}

void strrp (void)
{
  int a[10], c[10], e[10];
  mystruct *m = new mystruct;
  mystruct *&n = m;
  n->a = a;
  n->b = 3;
  n->c = c;
  n->d = 5;
  n->e = e;
  a[0] = 8;
  c[0] = 10;
  e[0] = 12;
  #pragma acc parallel copy(n->a[0:10], n->c[0:10], n->e[0:10])
  {
    n->a[0] = n->c[0] + n->e[0];
  }
  assert (n->a[0] == 10 + 12);
  delete m;
}

int main (int argc, char *argv[])
{
  str ();
  strp ();
  strr ();
  strrp ();
  return 0;
}
