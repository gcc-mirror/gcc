/* Check reductions over OpenMP threads on an AMD GCN device in "simt"
   offload mode, where each lane of a wavefront is a thread.  When all the
   lanes merge into the same variable, the compiler combines them across
   the wavefront first, and only one lane does the atomic update.  Check
   each supported type and operator, a combined "teams distribute parallel
   for" as in BabelStream, and nested regions, where each lane is a team of
   one with its own variable, so the lanes must not be combined.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 4096
#define MAXT 1024

int
main (void)
{
  for (int k = 0; k < 2; k++)
    {
      int nt = k ? 1024 : 64;
      int nthr = 0;
      long isum = 0, imin = N, imax = -1;
      unsigned iand = ~0U, ior = 0, ixor = 0;
      long long lprod = 1;
      double dsum = 0, dprod = 1;
      float fsum = 0;

      #pragma omp target teams num_teams(1) firstprivate(nt) \
		  map(tofrom: nthr, isum, imin, imax, iand, ior, ixor, \
			      lprod, dsum, dprod, fsum)
      #pragma omp parallel num_threads(nt)
      {
	#pragma omp single
	nthr = omp_get_num_threads ();

	#pragma omp for reduction(+: isum)
	for (int i = 0; i < N; i++)
	  isum += i;
	#pragma omp for reduction(min: imin)
	for (int i = 0; i < N; i++)
	  imin = (i + 7) % N < imin ? (i + 7) % N : imin;
	#pragma omp for reduction(max: imax)
	for (int i = 0; i < N; i++)
	  imax = (i * 3) % N > imax ? (i * 3) % N : imax;
	#pragma omp for reduction(&: iand)
	for (int i = 0; i < N; i++)
	  iand &= i == 100 ? 0xffff0fffU : ~0U;
	#pragma omp for reduction(|: ior)
	for (int i = 0; i < N; i++)
	  ior |= (i % 37 == 0) << (i % 32);
	#pragma omp for reduction(^: ixor)
	for (int i = 0; i < N; i++)
	  ixor ^= i;
	#pragma omp for reduction(*: lprod)
	for (int i = 0; i < N; i++)
	  lprod *= (i % 512 == 3 ? 3 : 1);
	#pragma omp for reduction(+: dsum)
	for (int i = 0; i < N; i++)
	  dsum += 0.5 * i;
	#pragma omp for reduction(*: dprod)
	for (int i = 0; i < N; i++)
	  dprod *= (i % 1024 == 1 ? 2.0 : 1.0);
	#pragma omp for reduction(+: fsum)
	for (int i = 0; i < N; i++)
	  fsum += 1.0f;
      }

      if (nthr != nt
	  || isum != (long) N * (N - 1) / 2
	  || imin != 0 || imax != N - 1
	  || iand != 0xffff0fffU
	  || lprod != 6561 || dsum != 0.25 * N * (N - 1)
	  || dprod != 16.0 || fsum != (float) N)
	abort ();

      unsigned eor = 0, exor = 0;
      for (int i = 0; i < N; i++)
	{
	  eor |= (i % 37 == 0) << (i % 32);
	  exor ^= i;
	}
      if (ior != eor || ixor != exor)
	abort ();

      /* Like BabelStream's "dot".  */
      double dot = 0;
      #pragma omp target teams distribute parallel for map(tofrom: dot) \
		  reduction(+: dot) num_teams(4) thread_limit(nt)
      for (int i = 0; i < N * 4; i++)
	dot += 2.0 * i;
      if (dot != 2.0 * (N * 4) * (N * 4 - 1) / 2)
	abort ();

      /* Nested regions: each lane is a team of one with its own SUM.  */
      int res[MAXT], bad = 0;
      #pragma omp target teams num_teams(1) firstprivate(nt) \
		  map(from: res) map(tofrom: bad)
      #pragma omp parallel num_threads(nt)
      {
	int t = omp_get_thread_num ();
	long sum = t, sum2 = t;
	#pragma omp parallel
	{
	  #pragma omp for reduction(+: sum)
	  for (int i = 0; i < 10; i++)
	    sum += i + t;
	}
	#pragma omp parallel reduction(+: sum2)
	sum2 += 45 + 10 * t;
	if (t < MAXT && sum == sum2)
	  res[t] = sum;
	else
	  {
	    #pragma omp atomic
	    bad++;
	  }
      }
      if (bad)
	abort ();
      for (int t = 0; t < nt; t++)
	if (res[t] != t + 45 + 10 * t)
	  abort ();
    }

  return 0;
}
