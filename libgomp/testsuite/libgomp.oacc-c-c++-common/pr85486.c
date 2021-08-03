/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-DVECTOR_LENGTH=vector_length(128)" } */

/* { dg-additional-options "-foffload=-fdump-tree-oaccloops" } */
/* { dg-set-target-env-var "GOMP_DEBUG" "1" } */

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

#pragma acc parallel copy (ary) VECTOR_LENGTH /* { dg-warning "using vector_length \\(32\\) due to call to vector-partitionable routine, ignoring 128" } */
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

/* { dg-final { scan-offload-tree-dump "__attribute__\\(\\(oacc function \\(1, 1, 32\\)" "oaccloops" } } */
/* { dg-output "nvptx_exec: kernel main\\\$_omp_fn\\\$0: launch gangs=1, workers=1, vectors=32" } */
