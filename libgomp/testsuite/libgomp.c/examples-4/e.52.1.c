/* { dg-do run } */

#include <stdlib.h>

const int MAX = 1800;

void check (int *a, int *b, int N)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] != b[i])
      abort ();
}

void init (int *a1, int *a2, int N)
{
  int i, s = -1;
  for (i = 0; i < N; i++)
    {
      a1[i] = s;
      a2[i] = i;
      s = -s;
    }
}

void init_again (int *a1, int *a2, int N)
{
  int i, s = -1;
  for (i = 0; i < N; i++)
    {
      a1[i] = s * 10;
      a2[i] = i;
      s = -s;
    }
}

void vec_mult_ref (int *p, int *v1, int *v2, int N)
{
  int i;

  init (v1, v2, MAX);

  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];

  init_again (v1, v2, N);

  for (i = 0; i < N; i++)
    p[i] = p[i] + (v1[i] * v2[i]);
}

void vec_mult (int *p, int *v1, int *v2, int N)
{
  int i;

  init (v1, v2, MAX);

  #pragma omp target data map(to: v1[:N], v2[:N]) map(from: p[0:N])
    {
      #pragma omp target
	#pragma omp parallel for
	  for (i = 0; i < N; i++)
	    p[i] = v1[i] * v2[i];

      init_again (v1, v2, N);

      #pragma omp target update to(v1[:N], v2[:N])

      #pragma omp target
	#pragma omp parallel for
	  for (i = 0; i < N; i++)
	    p[i] = p[i] + (v1[i] * v2[i]);
    }
}

int main ()
{
  int *p1 = (int *) malloc (MAX * sizeof (int));
  int *p2 = (int *) malloc (MAX * sizeof (int));
  int *v1 = (int *) malloc (MAX * sizeof (int));
  int *v2 = (int *) malloc (MAX * sizeof (int));

  vec_mult_ref (p1, v1, v2, MAX);
  vec_mult (p2, v1, v2, MAX);

  check (p1, p2, MAX);

  free (p1);
  free (p2);
  free (v1);
  free (v2);

  return 0;
}
