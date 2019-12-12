/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-set-target-env-var "GOMP_OPENACC_DIM" "::128" } */

/* Minimized from ref-1.C.  */

#include <stdio.h>

#pragma acc routine vector
void __attribute__((noinline, noclone))
Vector (int *ptr, int n, const int inc)
{
  #pragma acc loop vector
  for (unsigned ix = 0; ix < n; ix++)
    ptr[ix] += inc;
}

int
main (void)
{
  const int n = 32, m=32;

  int ary[m][n];
  unsigned ix,  iy;

  for (ix = m; ix--;)
    for (iy = n; iy--;)
      ary[ix][iy] = (1 << 16) + (ix << 8) + iy;

  int err = 0;

#pragma acc parallel copy (ary)
  {
    Vector (&ary[0][0], m * n, (1 << 24) - (1 << 16));
  }

  for (ix = m; ix--;)
    for (iy = n; iy--;)
      if (ary[ix][iy] != ((1 << 24) + (ix << 8) + iy))
	{
	  printf ("ary[%u][%u] = %x expected %x\n",
		  ix, iy, ary[ix][iy], ((1 << 24) + (ix << 8) + iy));
	  err++;
	}

  if (err)
    {
      printf ("%d failed\n", err);
      return 1;
    }

  return 0;
}

/* { dg-prune-output "using vector_length \\(32\\), ignoring runtime setting" } */
