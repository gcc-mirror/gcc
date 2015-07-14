/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 100
#define EPS 0.0000000000000001

#include <stdlib.h>

void init(double *a, double *a_ref, double *b, int n)
{
  int i, s = -1;
  for ( i = 0; i < n; i++ )
  {
    a[i] = i*i*s;
    a_ref[i] = a[i];
    b[i] = i+i;
    s = -s;
  }
}

double work( double *a, double *b, int n )
{
   int i;
   double tmp, sum;
   sum = 0.0;
   #pragma omp simd private(tmp) reduction(+:sum)
   for (i = 0; i < n; i++) {
      tmp = a[i] + b[i];
      sum += tmp;
   }
   return sum;
}

double work_ref( double *a, double *b, int n )
{
   int i;
   double tmp, sum;
   sum = 0.0;
   for (i = 0; i < n; i++) {
      tmp = a[i] + b[i];
      sum += tmp;
   }
   return sum;
}

int main ()
{
  double a[N], a_ref[N], b[N], res, ref, diff;

  init(a, a_ref, b, N);

  res = work(a, b, N);
  ref = work_ref(a_ref, b, N);

  diff = res - ref;

  if (diff > EPS || -diff > EPS)
    abort ();

  return 0;
}
