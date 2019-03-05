// { dg-do run }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

#define N 1024
int a[N], b[N];

int
f1 (void)
{
  int i;
  #pragma omp simd private (i)
  for (i = 0; i < N; i++)
    a[i] = b[i] * 2;
  #pragma omp simd lastprivate (i)
  for (i = 0; i < N; i++)
    a[i] += b[i] * 2;
  return i;
}

int
f2 (void)
{
  int i, j;
  #pragma omp simd private (i), collapse (2), lastprivate (j)
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; ++j)
      a[i * 32 + j] += b[i * 32 + j] * 2;
  return j;
}

int
f3 (void)
{
  static int i;
  #pragma omp for simd private (i)
  for (i = 0; i < N; ++i)
    a[i] = b[i] * 2;
  #pragma omp for simd lastprivate (i)
  for (i = 0; i < N; ++i)
    a[i] += b[i] * 2;
  return i;
}

int
f4 (void)
{
  static int i, j;
  #pragma omp for simd private (i)collapse (2)lastprivate (j)
  for (i = 0; i < 32; ++i)
    for (j = 0; j < 32; j++)
      a[i * 32 + j] += b[i * 32 + j] * 2;
  return j;
}

int
main ()
{
  int i;
  for (i = 0; i < N; ++i)
    a[i] = b[i] = i;
  if (f1 () != 1024 || f2 () != 32)
    __builtin_abort ();
  #pragma omp parallel num_threads(4)
  if (f3 () != 1024 || f4 () != 32)
    __builtin_abort ();
  for (i = 0; i < N; ++i)
    if (a[i] != 6 * i || b[i] != i)
      __builtin_abort ();
  return 0;
}
