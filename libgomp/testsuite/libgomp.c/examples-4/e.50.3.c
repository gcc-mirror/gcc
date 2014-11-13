/* { dg-do run } */

#include <stdlib.h>

#define N 100000

void init (long long *a1, long long *a2)
{
  long long s = -1;
  int i;
  for (i = 0; i < N; i++)
    {
      a1[i] = s;
      a2[i] = i;
      s = -s;
    }
}

void check (long long *a, long long *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] != b[i])
      abort ();
}

void vec_mult_ref (long long *p)
{
  int i;
  long long v1[N], v2[N];

  init (v1, v2);

  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (long long *p)
{
  int i;
  long long v1[N], v2[N];

  init (v1, v2);

  #pragma omp target map(v1, v2, p[0:N])
    #pragma omp parallel for
      for (i = 0; i < N; i++)
	p[i] = v1[i] * v2[i];
}

int main ()
{
  long long p1[N], p2[N];
  long long v1[N], v2[N];

  init (v1, v2);

  vec_mult_ref (p1);
  vec_mult (p2);

  check (p1, p2);

  return 0;
}
