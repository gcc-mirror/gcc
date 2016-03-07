/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

#include <stdlib.h>
#include <omp.h>

#define EPS 0.000001
#define THRESHOLD 1000

const int MAX = 1800;

void check (float *a, float *b, int N)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

void init (float *a1, float *a2, int N)
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

void init_again (float *a1, float *a2, int N)
{
  float s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s * 10;
      a2[i] = i;
      s = -s;
    }
}

void vec_mult_ref (float *p, float *v1, float *v2, int N)
{
  int i;

  init (v1, v2, N);

  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];

  init_again (v1, v2, N);

  for (i = 0; i < N; i++)
    p[i] = p[i] + (v1[i] * v2[i]);
}

void vec_mult (float *p, float *v1, float *v2, int N)
{
  int i;

  init (v1, v2, N);

  #pragma omp target data if(N > THRESHOLD) map(from: p[0:N])
    {
      #pragma omp target if (N > THRESHOLD) map(to: v1[:N], v2[:N])
	{
	  if (omp_is_initial_device ())
	    abort;

	  #pragma omp parallel for
	    for (i = 0; i < N; i++)
	      p[i] = v1[i] * v2[i];
	}

      init_again (v1, v2, N);

      #pragma omp target if (N > THRESHOLD) map(to: v1[:N], v2[:N])
	{
	  if (omp_is_initial_device ())
	    abort ();

	  #pragma omp parallel for
	    for (i = 0; i < N; i++)
	      p[i] = p[i] + (v1[i] * v2[i]);
	}
    }
}

int main ()
{
  float *p1 = (float *) malloc (MAX * sizeof (float));
  float *p2 = (float *) malloc (MAX * sizeof (float));
  float *v1 = (float *) malloc (MAX * sizeof (float));
  float *v2 = (float *) malloc (MAX * sizeof (float));

  vec_mult_ref (p1, v1, v2, MAX);
  vec_mult (p2, v1, v2, MAX);

  check (p1, p2, MAX);

  free (p1);
  free (p2);
  free (v1);
  free (v2);

  return 0;
}
