/* PR middle-end/106548.  */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

int a[256];

__attribute__((noinline, noclone)) int
f1 (int i)
{
  #pragma omp parallel for simd linear (i: 4)
  for (int j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f2 (short int i, char k)
{
  #pragma omp parallel for simd linear (i: k + 1)
  for (long j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) long long int
f3 (long long int i, long long int k)
{
  #pragma omp parallel for simd linear (i: k)
  for (short j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f4 (int i)
{
  #pragma omp parallel for simd linear (i: 4) schedule(static, 3)
  for (int j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f5 (short int i, char k)
{
  #pragma omp parallel for simd linear (i: k + 1) schedule(static, 5)
  for (long j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) long long int
f6 (long long int i, long long int k)
{
  #pragma omp parallel for simd linear (i: k) schedule(static, 7)
  for (short j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f7 (int i)
{
  #pragma omp parallel for simd linear (i: 4) schedule(dynamic, 3)
  for (int j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f8 (short int i, char k)
{
  #pragma omp parallel for simd linear (i: k + 1) schedule(dynamic, 5)
  for (long j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) long long int
f9 (long long int i, long long int k)
{
  #pragma omp parallel for simd linear (i: k) schedule(dynamic, 7)
  for (short j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f10 (int i, long step)
{
  #pragma omp parallel for simd linear (i: 4)
  for (int j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f11 (short int i, char k, char step)
{
  #pragma omp parallel for simd linear (i: k + 1)
  for (long j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) long long int
f12 (long long int i, long long int k, int step)
{
  #pragma omp parallel for simd linear (i: k)
  for (short j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f13 (int i, long long int step)
{
  #pragma omp parallel for simd linear (i: 4) schedule(static, 3)
  for (int j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f14 (short int i, char k, int step)
{
  #pragma omp parallel for simd linear (i: k + 1) schedule(static, 5)
  for (long j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) long long int
f15 (long long int i, long long int k, long int step)
{
  #pragma omp parallel for simd linear (i: k) schedule(static, 7)
  for (short j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f16 (int i, long long int step)
{
  #pragma omp parallel for simd linear (i: 4) schedule(dynamic, 3)
  for (int j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f17 (short int i, char k, int step)
{
  #pragma omp parallel for simd linear (i: k + 1) schedule(dynamic, 5)
  for (long j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) long long int
f18 (long long int i, long long int k, long int step)
{
  #pragma omp parallel for simd linear (i: k) schedule(dynamic, 7)
  for (short j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

int
main ()
{
#define TEST(x) \
  if (x != 8 + 48 * 4)				\
    __builtin_abort ();				\
  for (int i = 0; i < 256; i++)			\
    if (a[i] != (((i & 3) == 0 && i >= 8	\
		  && i < 8 + 48 * 4)		\
		 ? ((i - 8) / 4) + 16 : 0))	\
      __builtin_abort ();			\
  __builtin_memset (a, 0, sizeof (a))
  TEST (f1 (8));
  TEST (f2 (8, 3));
  TEST (f3 (8LL, 4LL));
  TEST (f4 (8));
  TEST (f5 (8, 3));
  TEST (f6 (8LL, 4LL));
  TEST (f7 (8));
  TEST (f8 (8, 3));
  TEST (f9 (8LL, 4LL));
  TEST (f10 (8, 2));
  TEST (f11 (8, 3, 2));
  TEST (f12 (8LL, 4LL, 2));
  TEST (f13 (8, 2));
  TEST (f14 (8, 3, 2));
  TEST (f15 (8LL, 4LL, 2));
  TEST (f16 (8, 2));
  TEST (f17 (8, 3, 2));
  TEST (f18 (8LL, 4LL, 2));
  return 0;
}
