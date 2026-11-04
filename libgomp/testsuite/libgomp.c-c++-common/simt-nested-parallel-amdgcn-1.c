/* Check a parallel region nested inside a parallel region on an AMD GCN
   device in "simt" offload mode, where each lane of a wavefront is an
   OpenMP thread.  The nested region is inactive, so each outer thread
   becomes a team of one: it must run the body once, with the right thread
   numbers and team sizes at both levels, and must run "single" bodies and
   all the iterations of a loop itself.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define MAXT 1024
#define N 100

int
main (void)
{
  int count[MAXT], sgl[MAXT], iters[MAXT], bad = 0, nthr = 0;

  for (int t = 0; t < MAXT; t++)
    count[t] = sgl[t] = iters[t] = 0;

  #pragma omp target teams num_teams(1) \
    map(tofrom: count, sgl, iters, bad, nthr)
  #pragma omp parallel
  {
    int t = omp_get_thread_num ();
    int n = omp_get_num_threads ();

    #pragma omp single
    nthr = n;

    if (t < 0 || t >= MAXT)
      {
	#pragma omp atomic
	bad++;
      }
    else
      {
	#pragma omp parallel
	{
	  #pragma omp atomic
	  count[t]++;

	  if (omp_get_level () != 2
	      || omp_get_active_level () != 1
	      || omp_get_num_threads () != 1
	      || omp_get_thread_num () != 0
	      || omp_get_ancestor_thread_num (0) != 0
	      || omp_get_ancestor_thread_num (1) != t
	      || omp_get_ancestor_thread_num (2) != 0
	      || omp_get_team_size (0) != 1
	      || omp_get_team_size (1) != n
	      || omp_get_team_size (2) != 1)
	    {
	      #pragma omp atomic
	      bad++;
	    }

	  #pragma omp single
	  sgl[t]++;

	  #pragma omp barrier

	  #pragma omp for
	  for (int i = 0; i < N; i++)
	    iters[t]++;
	}
      }
  }

  if (nthr < 1 || nthr > MAXT)
    abort ();
  if (bad != 0)
    abort ();
  for (int t = 0; t < MAXT; t++)
    if (count[t] != (t < nthr)
	|| sgl[t] != (t < nthr)
	|| iters[t] != (t < nthr ? N : 0))
      abort ();

  return 0;
}
