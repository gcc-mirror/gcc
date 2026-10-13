/* Check that reductions on a "#pragma acc loop vector" nested in a
   "#pragma acc parallel" region are computed correctly on AMD GCN, where
   the vector dimension is mapped onto the 64 SIMT lanes of a wavefront.

   Exercise both:
     - a trip count that is an exact multiple of the 64-lane wavefront
       width, so every SIMT group has all 64 lanes live, and
     - a trip count that is not, so the final SIMT group is executed
       with fewer than 64 lanes live (EXEC partially masked).  */

/* { dg-do run { target openacc_radeon_accel_selected } } */

#include <stdlib.h>

#define FULL_N (64 * 5)		/* Multiple of 64: only full groups.  */
#define PART_N (64 * 3 + 17)	/* Last group has only 17 lanes live.  */

static void
test_sum (int n)
{
  long sum = 0, expected = 0;
  int i;

  #pragma acc parallel copy(sum)
  #pragma acc loop vector reduction(+:sum)
  for (i = 0; i < n; i++)
    sum += i;

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

  #pragma acc parallel copy(rmin, rmax)
  #pragma acc loop vector reduction(min:rmin) reduction(max:rmax)
  for (i = 0; i < n; i++)
    {
      if (i < rmin)
	rmin = i;
      if (i > rmax)
	rmax = i;
    }

  if (rmin != 0 || rmax != n - 1)
    abort ();
}

static void
test_xor (int n)
{
  unsigned int x = 0, expected = 0;
  int i;

  #pragma acc parallel copy(x)
  #pragma acc loop vector reduction(^:x)
  for (i = 0; i < n; i++)
    x ^= i;

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
