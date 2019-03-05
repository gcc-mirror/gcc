/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-foffload=-fdump-tree-oaccdevlow" } */
/* We default to warp size 32 for the vector length, so the GOMP_OPENACC_DIM has
   no effect.  */
/* { dg-set-target-env-var "GOMP_OPENACC_DIM" "::128" } */
/* { dg-set-target-env-var "GOMP_DEBUG" "1" } */


#include <stdlib.h>

#define N 1024

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];
unsigned int n = N;

int
main (void)
{
  for (unsigned int i = 0; i < n; ++i)
    {
      a[i] = i % 3;
      b[i] = i % 5;
    }

#pragma acc parallel copyin (a,b) copyout (c)
  {
#pragma acc loop vector
    for (unsigned int i = 0; i < n; i++)
      c[i] = a[i] + b[i];
  }

  for (unsigned int i = 0; i < n; ++i)
    if (c[i] != (i % 3) + (i % 5))
      abort ();

  return 0;
}

/* { dg-final { scan-offload-tree-dump "__attribute__\\(\\(oacc function \\(1, 1, 32\\)" "oaccdevlow" } } */
/* { dg-output "nvptx_exec: kernel main\\\$_omp_fn\\\$0: launch gangs=1, workers=1, vectors=32" } */
