/* Check the static schedule of "unsigned long long" loops on an AMD GCN
   device in "simt" offload mode, where each lane of a wavefront is an
   OpenMP thread.  libgomp's static schedule for these loops is used by
   ordered loops and by schedule(runtime).  Every iteration must run exactly
   once, with a team of one wavefront and with several.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 3001
/* Above LONG_MAX, so that the compiler uses the GOMP_loop_ull_* entry
   points.  */
#define BASE (~0ULL - (1ULL << 33))

int
main (void)
{
  int cnt[N], bad = 0;

  for (int nt = 64; nt <= 1024; nt *= 16)
    for (int k = 0; k < 4; k++)
      {
	for (int i = 0; i < N; i++)
	  cnt[i] = 0;

	#pragma omp target map(tofrom: cnt, bad) firstprivate(nt, k)
	{
	  if (k == 2)
	    omp_set_schedule (omp_sched_static, 0);
	  else if (k == 3)
	    omp_set_schedule (omp_sched_static, 3);

	  #pragma omp parallel num_threads(nt)
	  {
	    if (k == 0)
	      {
		#pragma omp for ordered schedule(static)
		for (unsigned long long i = BASE; i < BASE + N; i++)
		  {
		    #pragma omp atomic
		    cnt[i - BASE]++;
		  }
	      }
	    else if (k == 1)
	      {
		#pragma omp for ordered schedule(static, 2)
		for (unsigned long long i = BASE + N - 1; i >= BASE; i--)
		  {
		    #pragma omp atomic
		    cnt[i - BASE]++;
		  }
	      }
	    else
	      {
		#pragma omp for schedule(runtime)
		for (unsigned long long i = BASE; i < BASE + N; i++)
		  {
		    #pragma omp atomic
		    cnt[i - BASE]++;
		  }
	      }
	  }
	}

	for (int i = 0; i < N; i++)
	  if (cnt[i] != 1)
	    bad++;
	if (bad)
	  abort ();
      }

  return 0;
}
