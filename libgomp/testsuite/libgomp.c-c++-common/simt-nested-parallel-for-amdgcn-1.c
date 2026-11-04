/* Check a combined "parallel for" nested inside a parallel region on an
   AMD GCN device in "simt" offload mode, where each lane of a wavefront is
   an OpenMP thread.  The nested region is inactive, so each outer thread
   is a team of one and must run all the iterations of its own loop, whose
   bounds differ between the lanes of a wavefront.  A loop with constant
   bounds uses the combined GOMP_parallel_loop_* entry points instead.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */
/* Each nested team, and each nested loop's per-lane ranges, comes from the
   team arena and is not returned to it, so the default arena runs out.  */
/* { dg-set-target-env-var GCN_TEAM_ARENA_SIZE "4194304" } */

#include <stdlib.h>
#include <omp.h>

#define MAXT 1024
#define NSCHED 8
#define CONSTN 17

/* The loop for outer thread T runs from T % 7 to T % 7 + T % 13 + 5, so
   both bounds vary.  */
#define LO(T) ((T) % 7)
#define HI(T) ((T) % 7 + (T) % 13 + 5)

int
main (void)
{
  int iters[NSCHED][MAXT], sum[NSCHED][MAXT], bad = 0, nthr = 0;

  for (int s = 0; s < NSCHED; s++)
    for (int t = 0; t < MAXT; t++)
      iters[s][t] = sum[s][t] = 0;

  #pragma omp target teams num_teams(1) map(tofrom: iters, sum, bad, nthr)
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
	int lo = LO (t), hi = HI (t);

	#pragma omp parallel for schedule(static)
	for (int i = lo; i < hi; i++)
	  {
	    iters[0][t]++;
	    sum[0][t] += i;
	  }

	#pragma omp parallel for schedule(static, 2)
	for (int i = lo; i < hi; i++)
	  {
	    iters[1][t]++;
	    sum[1][t] += i;
	  }

	#pragma omp parallel for schedule(dynamic)
	for (int i = lo; i < hi; i++)
	  {
	    iters[2][t]++;
	    sum[2][t] += i;
	  }

	#pragma omp parallel for schedule(dynamic, 3)
	for (int i = lo; i < hi; i++)
	  {
	    iters[3][t]++;
	    sum[3][t] += i;
	  }

	#pragma omp parallel for schedule(guided)
	for (int i = hi - 1; i >= lo; i--)
	  {
	    iters[4][t]++;
	    sum[4][t] += i;
	  }

	#pragma omp parallel for schedule(runtime)
	for (int i = lo; i < hi; i += 2)
	  {
	    iters[5][t]++;
	    sum[5][t] += i;
	  }

	#pragma omp parallel for schedule(dynamic, 2)
	for (int i = 0; i < CONSTN; i++)
	  {
	    iters[6][t]++;
	    sum[6][t] += i;
	  }

	unsigned long long ulo = lo, uhi = hi;
	#pragma omp parallel for schedule(dynamic)
	for (unsigned long long i = ulo; i < uhi; i++)
	  {
	    iters[7][t]++;
	    sum[7][t] += i;
	  }
      }
  }

  if (nthr < 1 || nthr > MAXT)
    abort ();
  if (bad != 0)
    abort ();
  for (int t = 0; t < MAXT; t++)
    for (int s = 0; s < NSCHED; s++)
      {
	int n = 0, total = 0;
	if (t < nthr)
	  for (int i = s == 6 ? 0 : LO (t); i < (s == 6 ? CONSTN : HI (t));
	       i += (s == 5 ? 2 : 1))
	    {
	      n++;
	      total += i;
	    }
	if (iters[s][t] != n || sum[s][t] != total)
	  abort ();
      }

  return 0;
}
