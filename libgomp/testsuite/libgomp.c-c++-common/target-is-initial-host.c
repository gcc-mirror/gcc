/* { dg-do run } */

/* Check whether 'omp_is_initial_device()' is properly compile-time optimized. */

/* { dg-additional-options "-fdump-tree-gimple -fdump-tree-optimized" }  */
/* { dg-additional-options -foffload-options=-fdump-tree-optimized { target { offload_target_nvptx || offload_target_amdgcn } } }  */

/* { dg-final { scan-tree-dump-times "omp_is_initial_device" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-not "omp_is_initial_device" "optimized" } }  */

/* { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump-not "omp_is_initial_device" "optimized" { target offload_target_amdgcn } } }  */
/* { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-not "omp_is_initial_device" "optimized" { target offload_target_nvptx } } }  */


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
