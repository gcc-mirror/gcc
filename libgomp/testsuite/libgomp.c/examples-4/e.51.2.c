/* { dg-do run } */

#include <stdlib.h>

const int MAX = 1800;

void check (char *a, char *b, int N)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] != b[i])
      abort ();
}

void init (char *a1, char *a2, int N)
{
  char s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s;
      a2[i] = i;
      s = -s;
    }
}

void init_again (char *a1, char *a2, int N)
{
  char s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s * 10;
      a2[i] = i;
      s = -s;
    }
}

void vec_mult_ref (char *p, char *v1, char *v2, int N)
{
  int i;

  init (v1, v2, N);

  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];

  init_again (v1, v2, N);

  for (i = 0; i < N; i++)
    p[i] = p[i] + (v1[i] * v2[i]);
}

void vec_mult (char *p, char *v1, char *v2, int N)
{
  int i;

  init (v1, v2, N);

  #pragma omp target data map(from: p[0:N])
    {
      #pragma omp target map(to: v1[:N], v2[:N])
	#pragma omp parallel for
	  for (i = 0; i < N; i++)
	    p[i] = v1[i] * v2[i];

      init_again (v1, v2, N);

      #pragma omp target map(to: v1[:N], v2[:N])
	#pragma omp parallel for
	  for (i = 0; i < N; i++)
	    p[i] = p[i] + (v1[i] * v2[i]);
    }
}

int main ()
{
  char *p1 = (char *) malloc (MAX * sizeof (char));
  char *p2 = (char *) malloc (MAX * sizeof (char));
  char *v1 = (char *) malloc (MAX * sizeof (char));
  char *v2 = (char *) malloc (MAX * sizeof (char));

  vec_mult_ref (p1, v1, v2, MAX);
  vec_mult (p2, v1, v2, MAX);

  check (p1, p2, MAX);

  free (p1);
  free (p2);
  free (v1);
  free (v2);

  return 0;
}
