/* { dg-do run } */

#include <stdlib.h>

#define N 100000

void init (char *a1, char *a2)
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

void check (char *a, char *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] != b[i])
      abort ();
}

void vec_mult_ref (char *p)
{
  int i;
  char v1[N], v2[N];

  init (v1, v2);

  for (i = 0; i < N; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (char *p)
{
  int i;
  char v1[N], v2[N];

  init (v1, v2);

  #pragma omp target map(from: p[0:N])
    #pragma omp parallel for
      for (i = 0; i < N; i++)
	p[i] = v1[i] * v2[i];
}

int main ()
{
  char p1[N], p2[N];
  char v1[N], v2[N];

  init (v1, v2);

  vec_mult_ref (p1);
  vec_mult (p2);

  check (p1, p2);

  return 0;
}
