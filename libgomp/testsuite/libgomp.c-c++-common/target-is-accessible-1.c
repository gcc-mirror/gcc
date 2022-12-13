/* { dg-do run } */

#include <omp.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int n = omp_get_num_devices ();
  int i = 42;
  void *p = &i;

  if (d < 0 || d >= n)
    d = id;

  if (!omp_target_is_accessible (p, sizeof (int), n))
    __builtin_abort ();

  if (!omp_target_is_accessible (p, sizeof (int), id))
    __builtin_abort ();

  if (!omp_target_is_accessible (p, sizeof (int), omp_initial_device))
    __builtin_abort ();

  if (omp_target_is_accessible (p, sizeof (int), -5))
    __builtin_abort ();

  if (omp_target_is_accessible (p, sizeof (int), n + 1))
    __builtin_abort ();

  int a[128];
  for (int d = 0; d <= omp_get_num_devices (); d++)
    {
      /* SHARED_MEM is 1 if and only if host and device share the same memory.
	 OMP_TARGET_IS_ACCESSIBLE should not return 0 for shared memory.  */
      int shared_mem = 0;
      #pragma omp target map (alloc: shared_mem) device (d)
	shared_mem = 1;

      if (shared_mem && !omp_target_is_accessible (p, sizeof (int), d))
	__builtin_abort ();

      /* USM is disabled by default.  Hence OMP_TARGET_IS_ACCESSIBLE should
	 return 0 if shared_mem is false.  */
      if (!shared_mem && omp_target_is_accessible (p, sizeof (int), d))
	__builtin_abort ();

      if (shared_mem && !omp_target_is_accessible (a, 128 * sizeof (int), d))
	__builtin_abort ();

      for (int i = 0; i < 128; i++)
	if (shared_mem && !omp_target_is_accessible (&a[i], sizeof (int), d))
	  __builtin_abort ();
    }

  return 0;
}
