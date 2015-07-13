/* { dg-do run { target vect_simd_clones } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include <stdio.h>
#include <stdlib.h>

#define N 45
int a[N], a_ref[N], b[N];

#pragma omp declare simd inbranch
int fib( int n )
{
   if (n <= 2)
      return n;
   else {
      return fib(n-1) + fib(n-2);
   }
}

int main(void)
{
  int i;

#pragma omp simd
  for (i=0; i < N; i++)
    b[i] = i;

#pragma omp simd
  for (i=0; i < N; i++)
    a[i] = fib(b[i]);

  for (i=0; i < N; i++)
    a_ref[i] = fib(b[i]);

  for (i=0; i < N; i++)
    if (a[i] != a_ref[i])
      abort ();

  return 0;
}
