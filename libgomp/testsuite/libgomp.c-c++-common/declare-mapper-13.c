/* { dg-do run } */

#include <assert.h>

struct T {
  int a;
  int b;
  int c;
};

void foo (void)
{
  struct T x;
  x.a = x.b = x.c = 0;

#pragma omp target
  {
    x.a++;
    x.c++;
  }

  assert (x.a == 1);
  assert (x.b == 0);
  assert (x.c == 1);
}

// An identity mapper.  This should do the same thing as the default!
#pragma omp declare mapper (struct T v) map(v)

void bar (void)
{
  struct T x;
  x.a = x.b = x.c = 0;

#pragma omp target
  {
    x.b++;
  }

#pragma omp target map(x)
  {
    x.a++;
  }

  assert (x.a == 1);
  assert (x.b == 1);
  assert (x.c == 0);
}

int main (int argc, char *argv[])
{
  foo ();
  bar ();
  return 0;
}
