/* { dg-do run } */
/* { dg-additional-options "-foffload-options=nvptx-none=-latomic" { target { offload_target_nvptx } } } */

#include <stdlib.h>

#define N 32
#define TYPE char

int
main (void)
{
  TYPE result = 1;
  TYPE a[N];
  for (int x = 0; x < N; ++x)
    a[x] = 1;

#pragma omp target map(tofrom: result) map(to:a)
#pragma omp for simd reduction(&&:result)
  for (int x = 0; x < N; ++x)
    result = result && a[x];

  if (result != 1)
    abort ();

  return 0;
}
