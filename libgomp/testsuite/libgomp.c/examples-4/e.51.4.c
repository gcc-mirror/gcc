/* { dg-do run } */

#include <stdlib.h>

#define EPS 0.000001

const int MAX = 1800;

void check (double *a, double *b, int N)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

void init (double *a1, double *a2, int N)
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

void vec_mult_ref (double *p1, double *v3, double *v4, int N)
{
  int i;
  for (i = 0; i < N; i++)
    p1[i] = v3[i] * v4[i];
}

void foo_ref (double *p0, double *v1, double *v2, int N)
{
  init (v1, v2, N);
  vec_mult_ref (p0, v1, v2, N);
}

void vec_mult (double *p1, double *v3, double *v4, int N)
{
  int i;
  #pragma omp target map(to: v3[0:N], v4[:N]) map(from: p1[0:N])
    #pragma omp parallel for
      for (i = 0; i < N; i++)
	p1[i] = v3[i] * v4[i];
}

void foo (double *p0, double *v1, double *v2, int N)
{
  init (v1, v2, N);

  #pragma omp target data map(to: v1[0:N], v2[:N]) map(from: p0[0:N])
    vec_mult (p0, v1, v2, N);
}

int main ()
{
  double *p1 = (double *) malloc (MAX * sizeof (double));
  double *p2 = (double *) malloc (MAX * sizeof (double));
  double *v1 = (double *) malloc (MAX * sizeof (double));
  double *v2 = (double *) malloc (MAX * sizeof (double));

  foo_ref (p1, v1, v2, MAX);
  foo (p2, v1, v2, MAX);

  check (p1, p2, MAX);

  free (p1);
  free (p2);
  free (v1);
  free (v2);

  return 0;
}
