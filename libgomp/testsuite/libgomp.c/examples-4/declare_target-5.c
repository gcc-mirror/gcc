/* { dg-do run { target vect_simd_clones } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include <stdlib.h>

#define EPS 0.00001
#define N 10000
#define M 1024

#pragma omp declare target
float Q[N][N];
#pragma omp declare simd uniform(i) linear(k) notinbranch
float Pfun (const int i, const int k)
{
  return Q[i][k] * Q[k][i];
}
#pragma omp end declare target

void init ()
{
  int i, j;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      Q[i][j] = 0.001 * i * j;
}

float accum_ref ()
{
  int i, k;
  float tmp = 0.0;

  for (i = 0; i < N; i++)
    {
      float tmp1 = 0.0;

      for (k = 0; k < M; k++)
	tmp1 += Pfun(i,k);

      tmp += tmp1;
    }

  return tmp;
}

float accum ()
{
  int i, k;
  float tmp = 0.0;

  #pragma omp target map(tofrom:tmp)
    #pragma omp parallel for reduction(+:tmp)
      for (i = 0; i < N; i++)
	{
	  float tmp1 = 0.0;

	  #pragma omp simd reduction(+:tmp1)
	    for (k = 0; k < M; k++)
	      tmp1 += Pfun(i,k);

	  tmp += tmp1;
	}

  return tmp;
}

void check (float a, float b)
{
  float err = (b == 0.0) ? a : (a - b) / b;
  if (((err > 0) ? err : -err) > EPS)
    abort ();
}

int main ()
{
  init ();

  #pragma omp target update to(Q)

  check (accum (), accum_ref ());

  return 0;
}
