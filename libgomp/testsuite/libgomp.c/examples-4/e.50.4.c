/* { dg-do run } */

#include <stdlib.h>

#define EPS 0.000001
#define N 100000

void init (double *a1, double *a2)
{
  double s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s;
      a2[i] = i;
      s = -s;
    }
}

void check (double *a, double *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

void vec_mult_ref (double *p, double *v1, double *v2)
{
  int i;
  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (double *p, double *v1, double *v2)
{
  int i;
  #pragma omp target map(to: v1[0:N], v2[:N]) map(from: p[0:N])
    #pragma omp parallel for
      for (i = 0; i < N; i++)
	p[i] = v1[i] * v2[i];
}

int main ()
{
  double p1[N], p2[N];
  double v1[N], v2[N];

  init (v1, v2);

  vec_mult_ref (p1, v1, v2);
  vec_mult (p2, v1, v2);

  check (p1, p2);

  return 0;
}
