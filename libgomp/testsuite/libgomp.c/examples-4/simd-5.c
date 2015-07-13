/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 128
#define M 16
#define EPS 0.0000000000000001
#define SAFELEN 16

#include <stdlib.h>

void init(double a[N][N], double b[N][N], int n)
{
  int i, j, s = -1;
  for (i = 0; i < n; i++)
  {
    for (j = 0; j < n; j++)
    {
       a[i][j] = i * j * s;
       b[i][j] = i + j + s;
       s = -s;
    }
  }
}

void work( double a[N][N], double b[N][N], double c[N][N], int n )
{
   int i, j;
   double tmp;
   #pragma omp for simd collapse(2) private(tmp)
   for (i = 0; i < n; i++)
   {
      for (j = 0; j < n; j++)
      {
         tmp = a[i][j] + b[i][j];
         c[i][j] = tmp;
      }
   }
}

void work_ref( double a[N][N], double b[N][N], double c[N][N], int n )
{
   int i, j;
   double tmp;
   for (i = 0; i < n; i++)
   {
      for (j = 0; j < n; j++)
      {
         tmp = a[i][j] + b[i][j];
         c[i][j] = tmp;
      }
   }
}

void check (double a[N][N], double b[N][N])
{
  int i, j;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (a[i][j] - b[i][j] > EPS || b[i][j] - a[i][j] > EPS)
        abort ();
}

int main ()
{
  double a[N][N], b[N][N], c[N][N], c_ref[N][N];

  init(a, b, N);

  work(a, b, c, N);
  work_ref(a, b, c_ref, N);

  check(c, c_ref);

  return 0;
}
