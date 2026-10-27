/* Check "#pragma omp single copyprivate" in a parallel region on an AMD GCN
   device in "simt" offload mode, where each lane of a wavefront is an
   OpenMP thread.  The body must run exactly once per construct, and every
   thread must receive the values, including the other lanes of the
   executing thread's own wavefront, which can't wait for it inside
   GOMP_single_copy_start.  In this mode the construct is lowered to
   GOMP_single_start plus GOMP_single_copy_wait instead.

   Each construct also contains an atomic, and the received values are
   tested after it, because those once caused the compiler to duplicate
   GOMP_single_copy_wait into both arms of the branch around the body.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define K 32
#define MAXT 1024

struct S
{
  int i;
  double d;
};

int
main (void)
{
  int count[K], bad = 0, nthr = 0;

  for (int k = 0; k < K; k++)
    count[k] = 0;

  #pragma omp target teams num_teams(1) map(tofrom: count, bad, nthr)
  #pragma omp parallel
  {
    #pragma omp single
    nthr = omp_get_num_threads ();

    for (int k = 0; k < K; k++)
      {
	int v = -1;
	long w = -1;
	struct S s = { -1, -1.0 };

	#pragma omp single copyprivate(v, w, s)
	{
	  #pragma omp atomic
	  count[k]++;
	  v = 1000 + k;
	  w = 2000L + omp_get_thread_num ();
	  s.i = 3000 + k;
	  s.d = k + 0.5;
	}

	/* W carries the executing thread's number, so it must be in range
	   and the same in every thread.  */
	if (v != 1000 + k
	    || w < 2000L || w >= 2000L + MAXT
	    || s.i != 3000 + k || s.d != k + 0.5)
	  {
	    #pragma omp atomic
	    bad++;
	  }
      }
  }

  if (nthr < 1 || nthr > MAXT)
    abort ();
  for (int k = 0; k < K; k++)
    if (count[k] != 1)
      abort ();
  if (bad != 0)
    abort ();

  return 0;
}

/* The lowering must make exactly one GOMP_single_copy_wait call per
   construct (one here), never duplicated.  */
/* { dg-final { scan-offload-tree-dump-times "GOMP_single_copy_wait \\(" 1 "optimized" { target offload_target_amdgcn } } } */
/* { dg-final { scan-offload-tree-dump-not "GOMP_single_copy_start" "optimized" { target offload_target_amdgcn } } } */
