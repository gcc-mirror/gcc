/* Check the active level on an AMD GCN device in "simt" offload mode, where
   each lane of a wavefront is an OpenMP thread.  A team of one wavefront
   has 64 threads, so it is active, and a parallel region nested inside it
   must get one thread.  Repeat the nested region to check that no other
   wavefronts are woken for it.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 100

int
main (void)
{
  for (int k = 0; k < 2; k++)
    {
      int nt = k ? 1024 : 64;
      int nthr = 0, bad = 0;

      #pragma omp target teams num_teams(1) map(tofrom: nthr, bad) \
		  firstprivate(nt)
      #pragma omp parallel num_threads(nt)
      {
	int t = omp_get_thread_num ();
	int b = 0;

	#pragma omp single
	nthr = omp_get_num_threads ();

	if (!omp_in_parallel () || omp_get_active_level () != 1
	    || omp_get_level () != 1)
	  b++;

	for (int i = 0; i < N; i++)
	  {
	    int r = 0, n = 0, lvl = 0, alvl = 0;

	    #pragma omp parallel reduction(+: r, n)
	    {
	      r += t + i;
	      n++;
	      lvl = omp_get_level ();
	      alvl = omp_get_active_level ();
	    }

	    if (r != t + i || n != 1 || lvl != 2 || alvl != 1)
	      b++;
	  }

	if (b)
	  {
	    #pragma omp atomic
	    bad += b;
	  }
      }

      if (nthr != nt || bad != 0)
	abort ();
    }

  return 0;
}
