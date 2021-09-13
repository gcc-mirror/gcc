/* { dg-do run } */
/* { dg-options "-fopenmp" } */

#include <omp.h>

int
main ()
{
  struct S { int a[7]; } s = { { 1, 2, 3, 4, 5, 6, 7 } };
  omp_event_handle_t x;
  #pragma omp parallel master
  #pragma omp task firstprivate (s) detach (x)
    {
      if (s.a[3] != 4)
	__builtin_abort ();
      omp_fulfill_event (x);
    }
  return 0;
}
