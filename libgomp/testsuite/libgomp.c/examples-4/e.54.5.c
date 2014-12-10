/* { dg-do run } */

#include <stdlib.h>

#define EPS 0.00001
#define N 10000

void init (float *a, float *b, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i] = 0.1 * i;
      b[i] = 0.01 * i * i;
    }
}

void vec_mult_ref (float *p, float *v1, float *v2, int n)
{
  int i;
  for (i = 0; i < n; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (float *p, float *v1, float *v2, int n)
{
  int i;
  #pragma omp target teams map(to: v1[0:n], v2[:n]) map(from: p[0:n])
    #pragma omp distribute simd
      for (i = 0; i < n; i++)
	p[i] = v1[i] * v2[i];
}

void check (float *a, float *b, int n)
{
  int i;
  for (i = 0 ; i < n ; i++)
    {
      float err = (a[i] == 0.0) ? b[i] : (b[i] - a[i]) / a[i];
      if (((err > 0) ? err : -err) > EPS)
	abort ();
    }
}

int main ()
{
  float *p1 = (float *) malloc (N * sizeof (float));
  float *p2 = (float *) malloc (N * sizeof (float));
  float *v1 = (float *) malloc (N * sizeof (float));
  float *v2 = (float *) malloc (N * sizeof (float));

  init (v1, v2, N);

  vec_mult_ref (p1, v1, v2, N);
  vec_mult (p2, v1, v2, N);

  check (p1, p2, N);

  free (p1);
  free (p2);
  free (v1);
  free (v2);

  return 0;
}
