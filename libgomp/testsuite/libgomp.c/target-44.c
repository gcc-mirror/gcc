/* { dg-additional-options "-foffload-options=nvptx-none=-latomic" { target { offload_target_nvptx } } } */

#include <stdlib.h>

struct s
{
  int i;
};

#pragma omp declare reduction(+: struct s: omp_out.i += omp_in.i)

int
main (void)
{
  const int N0 = 32768;

  struct s counter_N0 = { 0 };
#pragma omp target
#pragma omp for simd reduction(+: counter_N0)
  for (int i0 = 0 ; i0 < N0 ; i0++ )
    counter_N0.i += 1;

  if (counter_N0.i != N0)
    abort ();

  return 0;
}
