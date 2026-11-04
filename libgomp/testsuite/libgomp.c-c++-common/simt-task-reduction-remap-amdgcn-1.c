/* Check "in_reduction" on an AMD GCN device in "simt" offload mode, where
   each lane of a wavefront is an OpenMP thread.  GOMP_task_reduction_remap
   must give each thread its own private copy of a "parallel" task
   reduction, so the lanes of a wavefront must not share one.  The tasks are
   undeferred; deferred tasks are not supported in this mode yet.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

int
main (void)
{
  for (int nt = 64; nt <= 1024; nt *= 16)
    {
      long r = 0;
      double d = 1;
      int n = 0;

      #pragma omp target map(tofrom: r, d, n) firstprivate(nt)
      #pragma omp parallel num_threads(nt) reduction(task, +: r) \
		  reduction(task, *: d)
      {
	int t = omp_get_thread_num ();

	#pragma omp single
	n = omp_get_num_threads ();

	#pragma omp task in_reduction(+: r) in_reduction(*: d) if(0)
	{
	  r += t + 1;
	  d *= (t & 1) ? 2 : 0.5;
	}
	#pragma omp task in_reduction(+: r) if(0)
	r += 1000;
      }

      if (n != nt
	  || r != (long) n * (n + 1) / 2 + 1000L * n
	  || d != 1)
	abort ();
    }

  return 0;
}
