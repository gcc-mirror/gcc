/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int dev = omp_get_num_devices ();
  int x[6];
  omp_interop_t obj1 = omp_interop_none;
#pragma omp interop init(targetsync : obj1) depend(in : x) device(dev)
  if (obj1 != omp_interop_none)
    abort ();

#pragma omp interop use(obj1)
#pragma omp interop destroy(obj1) depend(out : x)
  if (obj1 != omp_interop_none)
    abort ();

  omp_set_default_device (dev);
  omp_interop_t obj2;

#pragma omp interop init(                                                      \
    target, targetsync,                                                        \
      prefer_type({fr("hip"), attr("ompx_gnu_prio:1", "ompx_gnu_debug")},      \
		    {attr("ompx_gnu_nicest"), attr("ompx_something")}) : obj1, \
      obj2) nowait
  if (obj1 != omp_interop_none || obj2 != omp_interop_none)
    abort ();
#pragma omp interop use(obj1, obj2) nowait

  omp_interop_t obj3 = __omp_interop_t_max__;

#pragma omp interop init(target : obj3) use(obj2) destroy(obj1) nowait
  if (obj1 != omp_interop_none || obj3 != omp_interop_none)
    abort ();
#pragma omp interop destroy(obj3, obj2) nowait
  if (obj2 != omp_interop_none || obj3 != omp_interop_none)
    abort ();

  return 0;
}
