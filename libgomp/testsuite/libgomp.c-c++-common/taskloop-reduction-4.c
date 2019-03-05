/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-std=c99" { target c } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include <omp.h>
#include <stdlib.h>

#define N 1024
long int u[N], m, n, o;

__attribute__((noipa)) void
foo (void)
{
  int i = -1;
  #pragma omp master taskloop simd reduction (+:m) grainsize (64)
  for (i = 0; i < N; ++i)
    m += u[i];
  if (i != (omp_get_thread_num () ? -1 : N))
    abort ();
}

__attribute__((noipa)) void
bar (int x)
{
  int i = -1;
  #pragma omp master taskloop simd in_reduction (+:n) grainsize (64)
  for (i = (x & 1) * (N / 2); i < (x & 1) * (N / 2) + (N / 2); i++)
    n += 2 * u[i];
  if (i != (omp_get_thread_num () ? -1 : (x & 1) * (N / 2) + (N / 2)))
    abort ();
}

__attribute__((noipa)) void
baz (void)
{
  int i;
  #pragma omp parallel master taskloop simd reduction (+:o) grainsize (64)
  for (i = 0; i < N; ++i)
    o += u[i];
  if (i != N)
    abort ();
}

int
main ()
{
  int i;
  for (i = 0; i < N; ++i)
    u[i] = i;
  #pragma omp parallel
  {
    foo ();
    #pragma omp taskgroup task_reduction (+:n)
    {
      bar (0);
      bar (1);
    }
  }
  baz ();
  if (m != (long)(N - 1) * (N / 2) || n != (long)(N - 1) * N || o != m)
    abort ();
  return 0;
}
