/* { dg-do run } */
/* { dg-additional-options "-fno-builtin-omp_is_initial_device" } */

/* Check whether 'omp_is_initial_device()' is NOT compile-time optimized. */

/* { dg-additional-options "-fdump-tree-gimple -fdump-tree-optimized" }  */
/* { dg-additional-options -foffload-options=-fdump-tree-optimized }  */

/* { dg-final { scan-tree-dump-times "omp_is_initial_device" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "omp_is_initial_device" 1 "optimized" } }  */

/* { dg-final { scan-offload-tree-dump-times "omp_is_initial_device" 1 "optimized" } }  */


#include <omp.h>

int
main ()
{
  int is_initial, dev_num, initial;
  initial = omp_get_initial_device();
  for (int dev = omp_initial_device; dev <= omp_get_num_devices(); dev++)
    {
      is_initial = dev_num = 99;
      #pragma omp target map(from: is_initial, dev_num) device(dev)
        {
          is_initial = omp_is_initial_device ();
          dev_num = omp_get_device_num ();
        }
      if (dev == omp_initial_device || dev == initial)
	{
	  if (dev_num != initial || is_initial != 1)
	    __builtin_abort ();
	}
      else
	{
	  if (dev_num != dev || is_initial != 0)
	    __builtin_abort ();
	}
    }
}
