/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 100
#define OFF 32
#define EPS 0.0000000000000001

#include <stdlib.h>

void init(double *a, double *a_ref, double *b, double *c, int n, int ioff)
{
  int i;
  for ( i = 0; i < n; i++ )
  {
    a[i] = i*i;
    a_ref[i] = a[i];
    b[i] = i+i;
  }

  int s = -1;
  for ( i = 0; i < n+ioff; i++ )
  {
    c[i] = s*3;
    s = -s;
  }
}

void star( double *a, double *b, double *c, int n, int *ioff )
{
  int i;
  #pragma omp simd
  for ( i = 0; i < n; i++ )
    a[i] *= b[i] * c[i+ *ioff];
}

void star_ref( double *a, double *b, double *c, int n, int *ioff )
{
  int i;
  for ( i = 0; i < n; i++ )
    a[i] *= b[i] * c[i+ *ioff];
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
  double a[N], a_ref[N], b[N], c[N+OFF];
  int ioff = OFF;

  init(a, a_ref, b, c, N, ioff);

  star(a, b, c, N, &ioff);
  star_ref(a_ref, b, c, N, &ioff);

  check(a, a_ref);

  return 0;
}
