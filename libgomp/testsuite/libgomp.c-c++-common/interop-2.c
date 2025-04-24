/* { dg-do run } */
/* { dg-additional-options "-lm" } */

/* Note: At the time this program was written, Nvptx was not asynchronous
   enough to trigger the issue (with a 'nowait' added); however, one
   AMD GPUs, it triggered.  */

/* Test whether nowait / dependency is handled correctly.
   Motivated by OpenMP_VV's 5.1/interop/test_interop_target.c

   The code actually only creates a streaming object without actually using it,
   except for dependency tracking.

   Note that there is a difference between having a steaming (targetsync) object
   and not (= omp_interop_none); at least if one assumes that omp_interop_none
   does not include 'targetsync' as (effective) interop type - in that case,
   'nowait' has no effect and the 'depend' is active as included task, otherwise
   the code continues with the depend being active only for the about to be
   destroyed or used thread.

   The OpenMP spec states (here 6.0):
     "If the interop-type set includes 'targetsync', an empty mergeable task is
      generated.  If the 'nowait' clause is not present on the construct then
      the task is also an included task. If the interop-type set does not
      include 'targetsync', the 'nowait' clause has no effect.  Any depend
      clauses that are present on the construct apply to the generated task.  */

#include <omp.h>

void
test_async (const int dev)
{
  constexpr int N = 2048;
  constexpr int ulp = 4;
  constexpr double M_PI = 2.0 * __builtin_acos (0.0);
  omp_interop_t obj1, obj2;
  double A[N] = { };
  int B[N] = { };

  /* Create interop object.  */
  #pragma omp interop device(dev) init(targetsync : obj1, obj2)

  if (dev == omp_initial_device || dev == omp_get_num_devices ())
    {
      if (obj1 != omp_interop_none || obj2 != omp_interop_none)
	__builtin_abort ();
    }
  else
    {
      if (obj1 == omp_interop_none || obj2 == omp_interop_none)
	__builtin_abort ();
    }

  /* DOUBLE */

  /* Now in the background update it, slowly enough that the
     code afterwards is reached while still running asynchronously.
     As OpenMP_VV's Issue #863 shows, the overhead is high enough to
     fail even when only doing an atomic integer increment.  */

  #pragma omp target device(dev) map(A) depend(out: A[:N]) nowait
  for (int i = 0; i < N; i++)
    #pragma omp atomic update
    A[i] += __builtin_sin (2*i*M_PI/N);

  /* DESTROY take care of the dependeny such that ... */

  if (obj1 == omp_interop_none)
    {
      // Same as below as 'nowait' is ignored.
      #pragma omp interop destroy(obj1) depend(in: A[:N]) nowait
    }
  else
    {
      #pragma omp interop destroy(obj1) depend(in: A[:N])
    }

  /* ... this code is only executed once the dependency as been fulfilled.  */

  /* Check the value - part I: quick, avoid A[0] == sin(0) = 0.  */
  for (int i = 1; i < N; i++)
    if (A[i] == 0.0)
      __builtin_abort ();

  /* Check the value - part II: throughly */
  for (int i = 0; i < N; i++)
    {
      double x = A[i];
      double y = __builtin_sin (2*i*M_PI/N);
      if (__builtin_fabs (x - y) > ulp * __builtin_fabs (x+y) * __DBL_EPSILON__)
	__builtin_abort ();
    }

  /* Integer */

  #pragma omp target device(dev) map(B) depend(out: B[:N]) nowait
  for (int i = 0; i < N; i++)
    #pragma omp atomic update
    B[i] += 42;

  /* Same - but using USE.  */
  if (obj2 == omp_interop_none)
    {
      // Same as below as 'nowait' is ignored.
      #pragma omp interop use(obj2) depend(in: B[:N]) nowait
    }
  else
    {
      #pragma omp interop use(obj2) depend(in: B[:N])
    }

  for (int i = 0; i < N; i++)
    if (B[i] != 42)
      __builtin_abort ();

  #pragma omp interop destroy(obj2)
}

int
main ()
{
  int ndev = omp_get_num_devices ();

  for (int dev = 0; dev <= ndev; dev++)
    test_async (dev);
  test_async (omp_initial_device);

  return 0;
}
