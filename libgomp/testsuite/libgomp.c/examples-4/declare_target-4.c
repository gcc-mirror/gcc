/* { dg-do run } */
/* { dg-additional-options "-DTESTITERS=20" { target { ! run_expensive_tests } } } */

#include <stdlib.h>

#define EPS 0.00001
#define N 1000
#ifndef TESTITERS
#define TESTITERS N
#endif

#pragma omp declare target
float Q[N][N];
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

float accum_ref (int k)
{
  int i;
  float tmp = 0.0;

  for (i = 0; i < N; i++)
    tmp += Pfun (i, k);

  return tmp;
}

float accum (int k)
{
  int i;
  float tmp = 0.0;

  #pragma omp target map(tofrom:tmp)
    #pragma omp parallel for reduction(+:tmp)
      for (i = 0; i < N; i++)
	tmp += Pfun (i, k);

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
  int i;

  init ();

  #pragma omp target update to(Q)

  for (i = 0; i < TESTITERS; i++)
    check (accum (i), accum_ref (i));

  return 0;
}
