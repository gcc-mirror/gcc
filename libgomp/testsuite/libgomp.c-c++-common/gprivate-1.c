#include <omp.h>

void
test_it (int dev)
{
  // Invalid access argument - expect 0
  if (omp_get_gprivate_limit (dev, (omp_access_t) -1) != 0)
    __builtin_abort ();

  // Invalid access argument - expect 0
  if (omp_get_gprivate_limit (dev, (omp_access_t) 99) != 0)
    __builtin_abort ();

  // FIXME: cgroup - should be valid in principle:
  // - offload: groupprivate permitted for offloading,
  //            even if dyn_groupprivate is not implemented.
  // - host: dyn_groupprivate implementable, but not implemented.
  //
  if (dev == omp_invalid_device
      && omp_get_gprivate_limit (dev, omp_access_cgroup) != 0)
    __builtin_abort ();
  else if (dev != omp_invalid_device  // For now, expect 0
	   && omp_get_gprivate_limit (dev, omp_access_cgroup) != 0)
    __builtin_abort ();

  // pteam - enum value defined but otherwise not specified at all!
  // but in principle the limit question could be answered.
  //
  // Still, for now assume == 0.
  if (omp_get_gprivate_limit (dev, omp_access_pteam) != 0)
    __builtin_abort ();

#if __cplusplus
  // access is optional and defaults to 'omp_access_cgroup'.
  if (omp_get_gprivate_limit (dev, omp_access_cgroup)
      != omp_get_gprivate_limit (dev))
    __builtin_abort ();
#endif
}

int main ()
{
 // avoid - will exit after printing: libgomp: omp_invalid_device encountered
 // test_it (omp_invalid_device);
 test_it (omp_default_device);

 for (int dev = omp_initial_device; dev <= omp_get_num_devices (); dev++)
   test_it (dev);
}
