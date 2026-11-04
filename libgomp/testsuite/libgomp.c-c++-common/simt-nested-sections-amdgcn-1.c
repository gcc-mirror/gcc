/* Check "sections" nested inside a parallel region on an AMD GCN device in
   "simt" offload mode, where each lane of a wavefront is an OpenMP thread.
   The nested region is inactive, so each outer thread is a team of one and
   must run every section itself, exactly once.  This covers the separate
   GOMP_sections_start/next entry points, the combined GOMP_parallel_sections,
   consecutive constructs with nowait, and a reduction.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */
/* Each nested team, and each nested construct's per-lane ranges, comes from
   the team arena and is not returned to it, so the default arena runs out.  */
/* { dg-set-target-env-var GCN_TEAM_ARENA_SIZE "4194304" } */

#include <stdlib.h>
#include <omp.h>

#define MAXT 1024
#define NCASE 5

int
main (void)
{
  int mask[NCASE][MAXT], cnt[NCASE][MAXT], red[MAXT], bad = 0, nthr = 0;

  for (int c = 0; c < NCASE; c++)
    for (int t = 0; t < MAXT; t++)
      mask[c][t] = cnt[c][t] = red[t] = 0;

  #pragma omp target teams num_teams(1) map(tofrom: mask, cnt, red, bad, nthr)
  #pragma omp parallel
  {
    int t = omp_get_thread_num ();

    #pragma omp single
    nthr = omp_get_num_threads ();

    if (t < 0 || t >= MAXT)
      {
	#pragma omp atomic
	bad++;
      }
    else
      {
	#pragma omp parallel
	{
	  #pragma omp sections
	  {
	    #pragma omp section
	    { mask[0][t] |= 1; cnt[0][t]++; }
	    #pragma omp section
	    { mask[0][t] |= 2; cnt[0][t]++; }
	    #pragma omp section
	    { mask[0][t] |= 4; cnt[0][t]++; }
	  }
	}

	#pragma omp parallel sections
	{
	  #pragma omp section
	  { mask[1][t] |= 1; cnt[1][t]++; }
	  #pragma omp section
	  { mask[1][t] |= 2; cnt[1][t]++; }
	  #pragma omp section
	  { mask[1][t] |= 4; cnt[1][t]++; }
	  #pragma omp section
	  { mask[1][t] |= 8; cnt[1][t]++; }
	}

	#pragma omp parallel
	{
	  #pragma omp sections nowait
	  {
	    #pragma omp section
	    { mask[2][t] |= 1; cnt[2][t]++; }
	    #pragma omp section
	    { mask[2][t] |= 2; cnt[2][t]++; }
	  }
	  #pragma omp sections nowait
	  {
	    #pragma omp section
	    { mask[3][t] |= 1; cnt[3][t]++; }
	    #pragma omp section
	    { mask[3][t] |= 2; cnt[3][t]++; }
	    #pragma omp section
	    { mask[3][t] |= 4; cnt[3][t]++; }
	    #pragma omp section
	    { mask[3][t] |= 8; cnt[3][t]++; }
	    #pragma omp section
	    { mask[3][t] |= 16; cnt[3][t]++; }
	  }
	}

	int r = 0;
	#pragma omp parallel sections reduction(+: r)
	{
	  #pragma omp section
	  { r += t; mask[4][t] |= 1; cnt[4][t]++; }
	  #pragma omp section
	  { r += 1000; mask[4][t] |= 2; cnt[4][t]++; }
	}
	red[t] = r;
      }
  }

  static const int full[NCASE] = { 7, 15, 3, 31, 3 };
  static const int count[NCASE] = { 3, 4, 2, 5, 2 };

  if (nthr < 1 || nthr > MAXT)
    abort ();
  if (bad != 0)
    abort ();
  for (int t = 0; t < MAXT; t++)
    {
      for (int c = 0; c < NCASE; c++)
	if (mask[c][t] != (t < nthr ? full[c] : 0)
	    || cnt[c][t] != (t < nthr ? count[c] : 0))
	  abort ();
      if (red[t] != (t < nthr ? t + 1000 : 0))
	abort ();
    }

  return 0;
}
