/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

#include <stdlib.h>
#include <omp.h>

#define THRESHOLD 1000

const int MAX = 1800;

void check (short *a, short *b, int N)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] != b[i])
      abort ();
}

void init (short *a1, short *a2, int N)
{
  short s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s;
      a2[i] = i;
      s = -s;
    }
}

void vec_mult_ref (short *p, short *v1, short *v2, int N)
{
  int i;
  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (short *p, short *v1, short *v2, int N)
{
  int i;
  #pragma omp target data map(from: p[0:N])
    #pragma omp target if (N > THRESHOLD) map(to: v1[:N], v2[:N])
      {
	if (omp_is_initial_device ())
	  abort ();
	#pragma omp parallel for
	  for (i = 0; i < N; i++)
	    p[i] = v1[i] * v2[i];
      }
}

int main ()
{
  short *p1 = (short *) malloc (MAX * sizeof (short));
  short *p2 = (short *) malloc (MAX * sizeof (short));
  short *v1 = (short *) malloc (MAX * sizeof (short));
  short *v2 = (short *) malloc (MAX * sizeof (short));

  init (v1, v2, MAX);

  vec_mult_ref (p1, v1, v2, MAX);
  vec_mult (p2, v1, v2, MAX);

  check (p1, p2, MAX);

  free (p1);
  free (p2);
  free (v1);
  free (v2);

  return 0;
}
