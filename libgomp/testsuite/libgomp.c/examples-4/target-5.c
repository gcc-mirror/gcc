/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

#include <omp.h>
#include <stdlib.h>

#define EPS 0.000001
#define N 100000
#define THRESHOLD1 10000
#define THRESHOLD2 1000

void init (float *a1, float *a2)
{
  float s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s;
      a2[i] = i;
      s = -s;
    }
}

void check (float *a, float *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

void vec_mult_ref (float *p, float *v1, float *v2)
{
  int i;
  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (float *p, float *v1, float *v2)
{
  int i;
  #pragma omp target if(N > THRESHOLD1) map(to: v1[0:N], v2[:N]) \
		     map(from: p[0:N])
    {
      if (omp_is_initial_device ())
	abort ();

      #pragma omp parallel for if(N > THRESHOLD2)
	for (i = 0; i < N; i++)
	  p[i] = v1[i] * v2[i];
    }
}

int main ()
{
  float p1[N], p2[N];
  float v1[N], v2[N];

  init (v1, v2);

  vec_mult_ref (p1, v1, v2);
  vec_mult (p2, v1, v2);

  check (p1, p2);

  return 0;
}
