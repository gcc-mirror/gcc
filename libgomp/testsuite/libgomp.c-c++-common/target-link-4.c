/* { dg-do run }  */

#include <stdint.h>
#include <omp.h>

#pragma omp requires self_maps

int A[3] = {-3,-4,-5};
static int q = -401;
#pragma omp declare target link(A, q)

#pragma omp begin declare target
void
f (uintptr_t *pA, uintptr_t *pq)
{
  if (A[0] != 1 || A[1] != 2 || A[2] != 3 || q != 42)
    __builtin_abort ();
  A[0] = 13;
  A[1] = 14;
  A[2] = 15;
  q = 23;
  *pA = (uintptr_t) &A[0];
  *pq = (uintptr_t) &q;
}
#pragma omp end declare target

int
main ()
{
  uintptr_t hpA = (uintptr_t) &A[0];
  uintptr_t hpq = (uintptr_t) &q;
  uintptr_t dpA, dpq;

  A[0] = 1;
  A[1] = 2;
  A[2] = 3;
  q = 42;

  for (int i = 0; i <= omp_get_num_devices (); ++i)
    {
      #pragma omp target device(device_num: i) map(dpA, dpq)
	f (&dpA, &dpq);
      if (hpA != dpA || hpq != dpq)
	__builtin_abort ();
      if (A[0] != 13 || A[1] != 14 || A[2] != 15 || q != 23)
	__builtin_abort ();
      A[0] = 1;
      A[1] = 2;
      A[2] = 3;
      q = 42;
    }
}
