/* Check "parallel" with task reductions (GOMP_parallel_reductions) on an
   AMD GCN device in "simt" offload mode, where each lane of a wavefront is
   an OpenMP thread.  At the top level, the private copies of every lane
   must be combined.  Nested inside a parallel region, each outer thread is
   a team of one with its own reductions, which every lane allocates at
   once; this needs newlib's SIMT malloc.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define MAXT 1024

int
main (void)
{
  int ires[MAXT], top_n = 0, nthr = 0, bad = 0;
  long top_r = 0;
  double dres[MAXT], top_d = 0;

  for (int t = 0; t < MAXT; t++)
    {
      ires[t] = 0;
      dres[t] = 0;
    }

  #pragma omp target map(tofrom: top_r, top_d, top_n)
  #pragma omp parallel reduction(task, +: top_r, top_d)
  {
    int t = omp_get_thread_num ();
    top_r += t + 1;
    top_d += 0.5;
    #pragma omp single
    top_n = omp_get_num_threads ();
  }

  if (top_n < 1 || top_n > MAXT
      || top_r != (long) top_n * (top_n + 1) / 2
      || top_d != top_n * 0.5)
    abort ();

  #pragma omp target teams num_teams(1) map(tofrom: ires, dres, nthr, bad)
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
	int r = 100;
	double d = 0.25;

	#pragma omp parallel reduction(task, +: r) reduction(task, *: d)
	{
	  if (omp_get_num_threads () != 1)
	    {
	      #pragma omp atomic
	      bad++;
	    }
	  r += t + 1;
	  d *= 4 + (t & 3);
	}
	ires[t] = r;
	dres[t] = d;
      }
  }

  if (nthr < 1 || nthr > MAXT)
    abort ();
  if (bad != 0)
    abort ();
  for (int t = 0; t < MAXT; t++)
    if (ires[t] != (t < nthr ? t + 101 : 0)
	|| dres[t] != (t < nthr ? 0.25 * (4 + (t & 3)) : 0))
      abort ();

  return 0;
}
