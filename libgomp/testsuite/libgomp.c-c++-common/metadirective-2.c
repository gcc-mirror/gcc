/* { dg-do run } */

#include <math.h>

#define N 100
#define EPSILON 0.001

#pragma omp declare target
void
f (double a[], double x) {
  int i;

  #pragma omp metadirective \
	when (construct={target}: distribute parallel for) \
	default (parallel for simd)
   for (i = 0; i < N; i++)
     a[i] = x * i;
}
#pragma omp end declare target

int
main (void)
{
  double a[N];
  int i;

  #pragma omp target teams map(from: a[0:N])
    f (a, M_PI);

  for (i = 0; i < N; i++)
    if (fabs (a[i] - (M_PI * i)) > EPSILON)
      return 1;

  f (a, M_E);

  for (i = 0; i < N; i++)
    if (fabs (a[i] - (M_E * i)) > EPSILON)
      return 1;

  return 0;
 }
