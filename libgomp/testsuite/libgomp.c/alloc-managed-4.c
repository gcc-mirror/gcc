/* { dg-do run } */
/* { dg-require-effective-target omp_managedmem } */
/* { dg-additional-options -foffload-options=amdgcn-amdhsa=-mxnack=on { target offload_target_amdgcn_with_xnack } } */
/* { dg-shouldfail "" } */
/* { dg-output "libgomp: attempted to free managed memory at 0x\[0-9a-f\]+, but the default device is set to the host device" } */

/* Check that omp_free emits an error if the default device has been changed
   to the host device.  */

#include <omp.h>
#include <stdint.h>

int
main ()
{
  int *a = (int *) omp_alloc(2 * sizeof(int), ompx_gnu_managed_mem_alloc);
  if (!a)
    __builtin_abort ();

  omp_set_default_device (omp_initial_device);
  omp_free(a, ompx_gnu_managed_mem_alloc);
  return 0;
}
