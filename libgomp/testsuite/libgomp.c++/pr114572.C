// PR c++/114572
// { dg-do run }
// { dg-options "-fopenmp -O0" }

#include <stdlib.h>

struct S
{
  S () : s (0) {}
  ~S () {}
  S operator= (const S &x) { s = x.s; return *this; }
  int s;
};

int
main ()
{
  S s;
  #pragma omp parallel for lastprivate(s)
  for (int i = 0; i < 10; ++i)
    s.s = i;
  if (s.s != 9)
    abort ();
}
