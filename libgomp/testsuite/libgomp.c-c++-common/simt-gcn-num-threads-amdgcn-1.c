/* Check GCN_NUM_THREADS on an AMD GCN device in "simt" offload mode, where
   each lane of a wavefront is a thread, so it counts lanes and need not be a
   multiple of the wavefront size.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */
/* { dg-set-target-env-var GCN_NUM_THREADS "100" } */

#include <stdlib.h>
#include <omp.h>

int
main (void)
{
  int n = 0, l = 0, count = 0;

  #pragma omp target map(tofrom: n, l, count)
  #pragma omp parallel
  {
    #pragma omp atomic
    count++;
    if (omp_get_thread_num () == 0)
      {
	n = omp_get_num_threads ();
	l = omp_get_thread_limit ();
      }
  }
  if (n != 100 || l != 100 || count != 100)
    abort ();

  return 0;
}
