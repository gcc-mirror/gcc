/* Check reductions that merge under a lock on an AMD GCN device in "simt"
   offload mode, where each lane of a wavefront is a thread: more than one
   reduction clause, array sections and user-defined reductions.  The lanes
   of a wavefront hold the lock together, so they must merge one at a time.
   Check full and partial wavefronts, worksharing loops, and nested regions,
   where each lane is a team of one.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 4096
#define M 8

struct pair { int lo, hi; };

#pragma omp declare reduction (pairmax : struct pair : \
  omp_out.lo = omp_in.lo < omp_out.lo ? omp_in.lo : omp_out.lo, \
  omp_out.hi = omp_in.hi > omp_out.hi ? omp_in.hi : omp_out.hi) \
  initializer (omp_priv = { 1 << 30, -1 })

static const int sizes[] = { 1, 64, 100, 1024 };

int
main (void)
{
  for (int k = 0; k < (int) (sizeof (sizes) / sizeof (sizes[0])); k++)
    {
      int nt = sizes[k];
      long a = 0, b = 0, c = 0;
      int mx = -1, arr[M] = { 0 };
      double d = 0;
      struct pair p = { 1 << 30, -1 };

      #pragma omp target teams num_teams(1) firstprivate(nt) \
		  map(tofrom: a, b, c, mx, arr, d, p)
      {
	/* Two clauses on a parallel region: one merge per thread.  */
	#pragma omp parallel num_threads(nt) reduction(+: a, b)
	{
	  a += omp_get_thread_num () + 1;
	  b += 2;
	}

	/* Mixed clauses on a worksharing loop.  */
	#pragma omp parallel num_threads(nt)
	#pragma omp for reduction(+: c, d) reduction(max: mx)
	for (int i = 0; i < N; i++)
	  {
	    c += i;
	    d += 0.5;
	    mx = i > mx ? i : mx;
	  }

	/* An array section.  */
	#pragma omp parallel for num_threads(nt) reduction(+: arr[:M])
	for (int i = 0; i < N; i++)
	  arr[i % M] += i;

	/* A user-defined reduction.  */
	#pragma omp parallel for num_threads(nt) reduction(pairmax: p)
	for (int i = 0; i < N; i++)
	  {
	    int v = (i * 7) % N;
	    p.lo = v < p.lo ? v : p.lo;
	    p.hi = v > p.hi ? v : p.hi;
	  }
      }

      if (a != (long) nt * (nt + 1) / 2 || b != 2L * nt)
	abort ();
      if (c != (long) N * (N - 1) / 2 || d != 0.5 * N || mx != N - 1)
	abort ();
      for (int j = 0; j < M; j++)
	{
	  long e = 0;
	  for (int i = j; i < N; i += M)
	    e += i;
	  if (arr[j] != e)
	    abort ();
	}
      if (p.lo != 0 || p.hi != N - 1)
	abort ();
    }

  /* Nested regions: each lane is a team of one, merging into its own
     variables.  */
  int bad = 0;
  #pragma omp target teams num_teams(1) map(tofrom: bad)
  #pragma omp parallel num_threads(128)
  {
    int t = omp_get_thread_num ();
    long x = t, y = 0;
    #pragma omp parallel reduction(+: x, y)
    {
      x += 10;
      y += 1;
    }
    if (x != t + 10 || y != 1)
      {
	#pragma omp atomic
	bad++;
      }
  }
  if (bad)
    abort ();

  return 0;
}
