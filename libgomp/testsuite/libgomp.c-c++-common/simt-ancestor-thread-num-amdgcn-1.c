/* Check omp_get_ancestor_thread_num and omp_get_team_size in a parallel
   region on an AMD GCN device in "simt" offload mode, where each lane of a
   wavefront is an OpenMP thread.  The initial thread (level 0) is not a
   SIMT team, so every lane must see ancestor thread 0 there, not its own
   lane number.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define MAXT 1024

int
main (void)
{
  int seen[MAXT], bad = 0, nthr = 0;

  for (int t = 0; t < MAXT; t++)
    seen[t] = 0;

  #pragma omp target teams num_teams(1) map(tofrom: seen, bad, nthr)
  #pragma omp parallel
  {
    int t = omp_get_thread_num ();
    int n = omp_get_num_threads ();

    #pragma omp single
    nthr = n;

    if (t < 0 || t >= MAXT
	|| omp_get_level () != 1
	|| omp_get_ancestor_thread_num (-1) != -1
	|| omp_get_ancestor_thread_num (0) != 0
	|| omp_get_ancestor_thread_num (1) != t
	|| omp_get_ancestor_thread_num (2) != -1
	|| omp_get_team_size (0) != 1
	|| omp_get_team_size (1) != n
	|| omp_get_team_size (2) != -1)
      {
	#pragma omp atomic
	bad++;
      }
    else
      {
	#pragma omp atomic
	seen[t]++;
      }
  }

  if (nthr < 1 || nthr > MAXT)
    abort ();
  if (bad != 0)
    abort ();
  for (int t = 0; t < MAXT; t++)
    if (seen[t] != (t < nthr))
      abort ();

  return 0;
}
