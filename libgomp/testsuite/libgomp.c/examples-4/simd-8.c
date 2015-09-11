/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include <stdlib.h>
#include <math.h>

#define EPS 0.005

int   P[1000];
float A[1000];

float do_work(float *arr)
{
  float pri;

#pragma omp simd lastprivate(pri)
  for (int i = 0; i < 999; ++i)
  {
    int j = P[i];

    pri = 0.5f;
    if (j % 2 == 0)
    {
      pri = A[j+1] + arr[i];
    }
    A[j] = pri * 1.5f;
    pri = pri + A[j];
  }

  return pri;
}

int main(void)
{
  float pri, arr[1000], diff;

  for (int i = 0; i < 1000; ++i)
  {
     P[i]   = i;
     A[i]   = i * 1.5f;
     arr[i] = i * 1.8f;
  }

  pri = do_work(&arr[0]);

  diff = pri - 8237.25;

  if (diff > EPS || -diff > EPS)
    abort ();

  return 0;
}
