/* { dg-do run } */

#include <stdlib.h>

#define EPS 0.000001
#define N 100000

#pragma omp declare target
float p1[N], p2[N], v1[N], v2[N];
#pragma omp end declare target

void init ()
{
  int i, s = -1;
  for (i = 0; i < N; i++)
    {
      v1[i] = s * 0.01;
      v2[i] = i;
      s = -s;
    }
}

void check ()
{
  int i;
  for (i = 0; i < N; i++)
    if (p1[i] - p2[i] > EPS || p2[i] - p1[i] > EPS)
      abort ();
}

void vec_mult_ref ()
{
  int i;
  for (i = 0; i < N; i++)
    p1[i] = v1[i] * v2[i];
}

void vec_mult ()
{
  int i;

  #pragma omp target update to(v1, v2)

  #pragma omp target
    #pragma omp parallel for
      for (i = 0; i < N; i++)
	p2[i] = v1[i] * v2[i];

  #pragma omp target update from(p2)
}

int main ()
{
  init ();

  vec_mult_ref ();
  vec_mult ();

  check ();

  return 0;
}
