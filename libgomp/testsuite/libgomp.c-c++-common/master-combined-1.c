/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-std=c99" { target c } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include <omp.h>
#include <stdlib.h>

#define N 64

int
main ()
{
  int p, *q, i, l;
  int a[N];
  q = a;
  #pragma omp parallel master num_threads(4) private (p) shared(a)
  {
    int i;
    p = omp_get_thread_num ();
    if (p != 0)
      abort ();
    #pragma omp taskloop nogroup
    for (i = 0; i < N; ++i)
      {
	if (omp_get_thread_num () >= 4)
	  abort ();
	a[i] = i;
      }
  }
  #pragma omp parallel num_threads(4)
  {
    #pragma omp master taskloop lastprivate (i, l) firstprivate (q)
    for (i = 0; i != N; i = i + 1)
      l = q[i];
  }
  if (i != N || l != N - 1)
    abort ();
  #pragma omp parallel master taskloop num_threads(4) \
		       lastprivate (i, l) firstprivate (q)
  for (i = 0; i < N - 5; i += 2)
    if (q[i] != i)
      abort ();
    else
      l = q[i];
  if (i != N - 4 || l != N - 6)
    abort ();
  #pragma omp parallel master taskloop simd num_threads(4)
  for (i = 0; i < N; i++)
    a[i] = 2 * a[i];
  if (i != N)
    abort ();
  #pragma omp parallel num_threads(4)
  {
    int j;
    #pragma omp master taskloop simd collapse(2)
    for (i = 0; i < 2; i += 2)
      for (j = 0; j < N; j++)
	a[j] = a[j] + 1;
  }
  for (i = 0; i < N; i++)
    if (a[i] != 2 * i + 1)
      abort ();
  return 0;
}
