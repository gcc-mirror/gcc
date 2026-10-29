/* Check the loop schedules on an AMD GCN device in "simt" offload mode,
   where each lane of a wavefront is an OpenMP thread.  Without a schedule
   clause, consecutive iterations go to consecutive threads, so that the
   lanes' memory accesses coalesce.  An explicit schedule(static) still
   gives each thread one contiguous block.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 3001

int tid[2][N];

int
main (void)
{
  for (int k = 0; k < 2; k++)
    {
      int nt = k ? 1024 : 64;
      int nthr = 0;

      #pragma omp target teams num_teams(1) map(from: tid, nthr) \
		  firstprivate(nt)
      #pragma omp parallel num_threads(nt)
      {
	int t = omp_get_thread_num ();

	#pragma omp single
	nthr = omp_get_num_threads ();

	#pragma omp for
	for (int i = 0; i < N; i++)
	  tid[0][i] = t;

	#pragma omp for schedule(static)
	for (int i = 0; i < N; i++)
	  tid[1][i] = t;
      }

      if (nthr != nt)
	abort ();

      int q = N / nthr, r = N % nthr;
      for (int i = 0; i < N; i++)
	{
	  int block = i < r * (q + 1) ? i / (q + 1) : r + (i - r * (q + 1)) / q;
	  if (tid[0][i] != i % nthr || tid[1][i] != block)
	    abort ();
	}
    }

  return 0;
}
