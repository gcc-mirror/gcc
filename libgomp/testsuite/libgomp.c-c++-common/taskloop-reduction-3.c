/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-std=c99" { target c } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 1024
long int u[N], m, n;

__attribute__((noipa)) void
foo (void)
{
  int i;
  #pragma omp taskloop simd reduction (+:m) grainsize (64)
  for (i = 0; i < N; ++i)
    m += u[i];
}

__attribute__((noipa)) void
bar (int x)
{
  #pragma omp taskloop simd in_reduction (+:n) grainsize (64) nogroup
  for (int i = (x & 1) * (N / 2); i < (x & 1) * (N / 2) + (N / 2); i++)
    n += 2 * u[i];
}

int
main ()
{
  int i;
  for (i = 0; i < N; ++i)
    u[i] = i;
  #pragma omp parallel master
  {
    foo ();
    #pragma omp taskgroup task_reduction (+:n)
    {
      bar (0);
      bar (1);
    }
  }
  if (m != (long)(N - 1) * (N / 2) || n != (long)(N - 1) * N)
    __builtin_abort ();
  return 0;
}
