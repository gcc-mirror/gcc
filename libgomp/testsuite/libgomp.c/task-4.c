/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>
#include <string.h>

int e;

void __attribute__((noinline))
baz (int i, int *p, int j, int *q)
{
  if (p[0] != 1 || p[i] != 3 || q[0] != 2 || q[j] != 4)
    #pragma omp atomic
      e++;
}

void __attribute__((noinline))
foo (int i, int j)
{
  int p[i + 1];
  int q[j + 1];
  memset (p, 0, sizeof (p));
  memset (q, 0, sizeof (q));
  p[0] = 1;
  p[i] = 3;
  q[0] = 2;
  q[j] = 4;
  #pragma omp task firstprivate (p, q)
    baz (i, p, j, q);
}

int
main (void)
{
  #pragma omp parallel num_threads (4)
    foo (5 + omp_get_thread_num (), 7 + omp_get_thread_num ());
  if (e)
    abort ();
  return 0;
}
