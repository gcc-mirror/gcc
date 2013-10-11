#include <stdlib.h>
#include <omp.h>

struct S
{
  static int s;
  int v;
  S ()
  {
    #pragma omp atomic
    s++;
  }

  S (int x)
  {
    #pragma omp atomic
    s++;
    v = x;
  }

  ~S ()
  {
    #pragma omp atomic
    s--;
  }

  S (const S &x)
  {
    #pragma omp atomic
    s++;
    v = x.v;
  }

  static void
  verify ()
  {
    if (s) abort ();
  }

  void
  bump ()
  {
    v++;
  }
};

int S::s = 0;
