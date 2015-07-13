/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 128
#define M 16
#define EPS 0.0000000000000001
#define SAFELEN 16

#include <stdlib.h>

void init(double *a, double *b, int n)
{
  int i, s = -1;
  for ( i = 0; i < n; i++ )
  {
    a[i] = i*i*s;
    b[i] = a[i];
    s = -s;
  }
}

void work( double *b, int n, int m )
{
   int i;
   #pragma omp simd safelen(SAFELEN)
   for (i = m; i < n; i++)
      b[i] = b[i-m] - 1.0f;
}

void work_ref( double *b, int n, int m )
{
   int i;
   for (i = m; i < n; i++)
      b[i] = b[i-m] - 1.0f;
}

void check (double *a, double *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

int main ()
{
  double b[N], b_ref[N];

  init(b, b_ref, N);

  work(b, N, M);
  work(b_ref, N, M);

  check(b, b_ref);

  return 0;
}
