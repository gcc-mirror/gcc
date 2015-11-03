int a[256];

__attribute__((noinline, noclone)) int
f1 (int i)
{
  #pragma omp parallel for linear (i: 4)
  for (int j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int &
f2 (short int &i, char k)
{
  #pragma omp parallel for linear (i: k + 1)
  for (long j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

template <typename T>
__attribute__((noinline, noclone)) T
f3 (T i, T k)
{
  #pragma omp parallel for linear (i: k)
  for (short j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

template <typename T>
__attribute__((noinline, noclone)) T &
f4 (T &i)
{
  #pragma omp parallel for linear (i: 4) schedule(static, 3)
  for (int j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f5 (short int i, char &k)
{
  #pragma omp parallel for linear (i: k + 1) schedule(static, 5)
  for (long j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

template <int N>
__attribute__((noinline, noclone)) long long int
f6 (long long int i, long long int k)
{
  #pragma omp parallel for linear (i: k) schedule(static, 7)
  for (short j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f7 (int &i)
{
  #pragma omp parallel for linear (i: 4) schedule(dynamic, 3)
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
  #pragma omp parallel for linear (i: k + 1) schedule(dynamic, 5)
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
  #pragma omp parallel for linear (i: k) schedule(dynamic, 7)
  for (short j = 16; j < 64; j++)
    {
      a[i] = j;
      i += 4;
    }
  return i;
}

template <typename T>
__attribute__((noinline, noclone)) T &
f10 (T &i, long &step)
{
  #pragma omp parallel for linear (i: 4)
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
  #pragma omp parallel for linear (i: k + 1)
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
  #pragma omp parallel for linear (i: k)
  for (short j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) int
f13 (int &i, long long int step)
{
  #pragma omp parallel for linear (i: 4) schedule(static, 3)
  for (int j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

__attribute__((noinline, noclone)) short int
f14 (short int &i, char &k, int &step)
{
  #pragma omp parallel for linear (i: k + 1) schedule(static, 5)
  for (long j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

template <int N>
__attribute__((noinline, noclone)) long long int
f15 (long long int i, long long int k, long int step)
{
  #pragma omp parallel for linear (i: k) schedule(static, 7)
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
  #pragma omp parallel for linear (i: 4) schedule(dynamic, 3)
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
  #pragma omp parallel for linear (i: k + 1) schedule(dynamic, 5)
  for (long j = 16; j < 112; j += step)
    {
      a[i] = j / 2 + 8;
      i += 4;
    }
  return i;
}

template <typename T>
__attribute__((noinline, noclone)) T
f18 (T i, T k, long int step)
{
  #pragma omp parallel for linear (i: k) schedule(dynamic, 7)
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
  short int vs = 8;
  TEST (f2 (vs, 3));
  TEST (f3 (8LL, 4LL));
  int vi = 8;
  TEST (f4 (vi));
  char vk = 3;
  TEST (f5 (8, vk));
  TEST (f6<7> (8LL, 4LL));
  vi = 8;
  TEST (f7 (vi));
  TEST (f8 (8, 3));
  TEST (f9 (8LL, 4LL));
  vi = 8;
  long vl = 2;
  TEST (f10 (vi, vl));
  TEST (f11 (8, 3, 2));
  TEST (f12 (8LL, 4LL, 2));
  vi = 8;
  TEST (f13 (vi, 2));
  vs = 8;
  vk = 3;
  vi = 2;
  TEST (f14 (vs, vk, vi));
  TEST (f15<9> (8LL, 4LL, 2));
  TEST (f16 (8, 2));
  TEST (f17 (8, 3, 2));
  long long int vll1 = 8LL;
  long long int vll2 = 4LL;
  TEST (f18<long long int &> (vll1, vll2, 2));
  return 0;
}
