/* Companion to simt-reduction-amdgcn-1.c: check reductions on a
   "#pragma acc loop vector" nested inside gang/worker parallelism
   ("#pragma acc parallel" + "#pragma acc loop worker"), which is the
   OpenACC analogue of simt-reduction-amdgcn-2.c's OpenMP
   "teams distribute parallel for simd" case, as opposed to the bare
   "#pragma acc parallel" + "#pragma acc loop vector" with no enclosing
   worker loop exercised by simt-reduction-amdgcn-1.c.

   num_gangs(1) num_workers(1) forces a single gang and a single worker,
   i.e. a single wavefront, so this isolates the within-wavefront
   reduction tree from any cross-gang/cross-worker combination -- the
   same thing simt-reduction-amdgcn-1.c checks, just reached through the
   nested construct instead of the bare one.  */

/* { dg-do run { target openacc_radeon_accel_selected } } */

#include <stdlib.h>

#define FULL_N (64 * 5)		/* Multiple of 64: only full groups.  */
#define PART_N (64 * 3 + 17)	/* Last group has only 17 lanes live.  */

static void
test_sum (int n)
{
  long sum = 0, expected = 0;
  int i;

  #pragma acc parallel num_gangs(1) num_workers(1) copy(sum)
  #pragma acc loop worker
  for (int w = 0; w < 1; w++)
    {
      #pragma acc loop vector reduction(+:sum)
      for (i = 0; i < n; i++)
	sum += i;
    }

  for (i = 0; i < n; i++)
    expected += i;

  if (sum != expected)
    abort ();
}

static void
test_minmax (int n)
{
  int rmin = n, rmax = -1;
  int i;

  #pragma acc parallel num_gangs(1) num_workers(1) copy(rmin, rmax)
  #pragma acc loop worker
  for (int w = 0; w < 1; w++)
    {
      #pragma acc loop vector reduction(min:rmin) reduction(max:rmax)
      for (i = 0; i < n; i++)
	{
	  if (i < rmin)
	    rmin = i;
	  if (i > rmax)
	    rmax = i;
	}
    }

  if (rmin != 0 || rmax != n - 1)
    abort ();
}

static void
test_xor (int n)
{
  unsigned int x = 0, expected = 0;
  int i;

  #pragma acc parallel num_gangs(1) num_workers(1) copy(x)
  #pragma acc loop worker
  for (int w = 0; w < 1; w++)
    {
      #pragma acc loop vector reduction(^:x)
      for (i = 0; i < n; i++)
	x ^= i;
    }

  for (i = 0; i < n; i++)
    expected ^= i;

  if (x != expected)
    abort ();
}

int
main (void)
{
  test_sum (FULL_N);
  test_sum (PART_N);
  test_minmax (FULL_N);
  test_minmax (PART_N);
  test_xor (FULL_N);
  test_xor (PART_N);
  return 0;
}
