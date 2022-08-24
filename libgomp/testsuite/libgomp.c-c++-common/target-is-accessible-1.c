#include <omp.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int n = omp_get_num_devices ();
  void *p;

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

  /* Currently, a host pointer is accessible if the device supports shared
     memory or omp_target_is_accessible is executed on the host. This
     test case must be adapted when unified shared memory is avialable.  */
  int a[128];
  for (int d = 0; d <= omp_get_num_devices (); d++)
    {
      int shared_mem = 0;
      #pragma omp target map (alloc: shared_mem) device (d)
	shared_mem = 1;
      if (omp_target_is_accessible (p, sizeof (int), d) != shared_mem)
	__builtin_abort ();

      if (omp_target_is_accessible (a, 128 * sizeof (int), d) != shared_mem)
	__builtin_abort ();

      for (int i = 0; i < 128; i++)
	if (omp_target_is_accessible (&a[i], sizeof (int), d) != shared_mem)
	  __builtin_abort ();
    }

  return 0;
}
