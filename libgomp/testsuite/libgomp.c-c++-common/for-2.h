#ifndef VARS
#define VARS
int a[1500];
float b[10][15][10];
__attribute__((noreturn)) void
noreturn (void)
{
  for (;;);
}
#endif
#ifndef SC
#define SC
#endif
#ifndef OMPTGT
#define OMPTGT
#endif
#ifndef OMPTEAMS
#define OMPTEAMS
#endif
#ifndef OMPTO
#define OMPTO(v) do {} while (0)
#endif
#ifndef OMPFROM
#define OMPFROM(v) do {} while (0)
#endif

#ifndef CONDNE
__attribute__((noinline, noclone)) void
N(f0) (void)
{
  int i;
  OMPTGT
#pragma omp F S
  for (i = 0; i < 1500; i++)
    a[i] += 2;
}

__attribute__((noinline, noclone)) void
N(f1) (void)
{
  OMPTGT
#pragma omp F S
  for (unsigned int i = __INT_MAX__; i < 3000U + __INT_MAX__; i += 2)
    a[(i - __INT_MAX__) >> 1] -= 2;
}

__attribute__((noinline, noclone)) void
N(f2) (void)
{
  unsigned long long i;
  OMPTGT
#pragma omp F S
  for (i = __LONG_LONG_MAX__ + 4500ULL - 27;
       i > __LONG_LONG_MAX__ - 27ULL; i -= 3)
    a[(i + 26LL - __LONG_LONG_MAX__) / 3] -= 4;
}

__attribute__((noinline, noclone)) void
N(f3) (long long n1, long long n2, long long s3)
{
  OMPTGT
#pragma omp F S
  for (long long i = n1 + 23; i > n2 - 25; i -= s3)
    a[i + 48] += 7;
}

__attribute__((noinline, noclone)) void
N(f4) (void)
{
  unsigned int i;
  OMPTGT
#pragma omp F S
  for (i = 30; i < 20; i += 2)
    a[i] += 10;
}

__attribute__((noinline, noclone)) void
N(f5) (int n11, int n12, int n21, int n22, int n31, int n32,
       int s1, int s2, int s3)
{
  SC int v1, v2, v3;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = n11; v1 < n12; v1 += s1)
    for (v2 = n21; v2 < n22; v2 += s2)
      for (v3 = n31; v3 < n32; v3 += s3)
	b[v1][v2][v3] += 2.5;
}

__attribute__((noinline, noclone)) void
N(f6) (int n11, int n12, int n21, int n22, long long n31, long long n32,
       int s1, int s2, long long int s3)
{
  SC int v1, v2;
  SC long long v3;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = n11; v1 > n12; v1 += s1)
    for (v2 = n21; v2 > n22; v2 += s2)
      for (v3 = n31; v3 > n32; v3 += s3)
	b[v1][v2 / 2][v3] -= 4.5;
}

__attribute__((noinline, noclone)) void
N(f7) (void)
{
  SC unsigned int v1, v3;
  SC unsigned long long v2;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = 0; v1 < 20; v1 += 2)
    for (v2 = __LONG_LONG_MAX__ + 16ULL;
	 v2 > __LONG_LONG_MAX__ - 29ULL; v2 -= 3)
      for (v3 = 10; v3 > 0; v3--)
	b[v1 >> 1][(v2 - __LONG_LONG_MAX__ + 64) / 3 - 12][v3 - 1] += 5.5;
}

__attribute__((noinline, noclone)) void
N(f8) (void)
{
  SC long long v1, v2, v3;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = 0; v1 < 20; v1 += 2)
    for (v2 = 30; v2 < 20; v2++)
      for (v3 = 10; v3 < 0; v3--)
	b[v1][v2][v3] += 5.5;
}

__attribute__((noinline, noclone)) void
N(f9) (void)
{
  int i;
  OMPTGT
#pragma omp F S
  for (i = 20; i < 10; i++)
    {
      a[i] += 2;
      noreturn ();
      a[i] -= 4;
    }
}

__attribute__((noinline, noclone)) void
N(f10) (void)
{
  SC int i;
  OMPTGT
#pragma omp F S collapse(3)
  for (i = 0; i < 10; i++)
    for (int j = 10; j < 8; j++)
      for (long k = -10; k < 10; k++)
	{
	  b[i][j][k] += 4;
	  noreturn ();
	  b[i][j][k] -= 8;
	}
}

__attribute__((noinline, noclone)) void
N(f11) (int n)
{
  int i;
  OMPTGT
#pragma omp F S
  for (i = 20; i < n; i++)
    {
      a[i] += 8;
      noreturn ();
      a[i] -= 16;
    }
}

__attribute__((noinline, noclone)) void
N(f12) (int n)
{
  SC int i;
  OMPTGT
#pragma omp F S collapse(3)
  for (i = 0; i < 10; i++)
    for (int j = n; j < 8; j++)
      for (long k = -10; k < 10; k++)
	{
	  b[i][j][k] += 16;
	  noreturn ();
	  b[i][j][k] -= 32;
	}
}

__attribute__((noinline, noclone)) void
N(f13) (void)
{
  int *i;
  OMPTGT
#pragma omp F S
  for (i = a; i < &a[1500]; i++)
    i[0] += 2;
}

__attribute__((noinline, noclone)) void
N(f14) (void)
{
  SC float *i;
  OMPTGT
#pragma omp F S collapse(3)
  for (i = &b[0][0][0]; i < &b[0][0][10]; i++)
    for (float *j = &b[0][15][0]; j > &b[0][0][0]; j -= 10)
      for (float *k = &b[0][0][10]; k > &b[0][0][0]; --k)
	b[i - &b[0][0][0]][(j - &b[0][0][0]) / 10 - 1][(k - &b[0][0][0]) - 1]
	  -= 3.5;
}

__attribute__((noinline, noclone)) int
N(test) (void)
{
  int i, j, k;
  for (i = 0; i < 1500; i++)
    a[i] = i - 25;
  OMPTO (a);
  OMPTEAMS
  N(f0) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 23)
      return 1;
  OMPTEAMS
  N(f1) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 25)
      return 1;
  OMPTEAMS
  N(f2) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 29)
      return 1;
  OMPTEAMS
  N(f3) (1500LL - 1 - 23 - 48, -1LL + 25 - 48, 1LL);
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  OMPTEAMS
  N(f3) (1500LL - 1 - 23 - 48, 1500LL - 1, 7LL);
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  OMPTEAMS
  N(f4) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	b[i][j][k] = i - 2.5 + 1.5 * j - 1.5 * k;
  OMPTO (b);
  OMPTEAMS
  N(f5) (0, 10, 0, 15, 0, 10, 1, 1, 1);
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f5) (0, 10, 30, 15, 0, 10, 4, 5, 6);
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f6) (9, -1, 29, 0, 9, -1, -1, -2, -1);
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i - 4.5 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f7) ();
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.0 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f8) ();	  
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.0 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f9) ();
  OMPTEAMS
  N(f10) ();
  OMPTEAMS
  N(f11) (10);
  OMPTEAMS
  N(f12) (12);
  OMPFROM (a);
  OMPFROM (b);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.0 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f13) ();
  OMPTEAMS
  N(f14) ();
  OMPFROM (a);
  OMPFROM (b);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 20)
      return 1;
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i - 2.5 + 1.5 * j - 1.5 * k)
	  return 1;
  return 0;
}

#else

__attribute__((noinline, noclone)) void
N(f20) (void)
{
  int i;
  OMPTGT
#pragma omp F S
  for (i = 0; i != 1500; i++)
    a[i] += 2;
}

__attribute__((noinline, noclone)) void
N(f21) (void)
{
  OMPTGT
#pragma omp F S
  for (unsigned int i = __INT_MAX__; i < 1500U + __INT_MAX__; i += 2 - 1)
    a[(i - __INT_MAX__)] -= 2;
}

__attribute__((noinline, noclone)) void
N(f22) (void)
{
  unsigned long long i;
  OMPTGT
#pragma omp F S
  for (i = __LONG_LONG_MAX__ + 1500ULL - 27;
       i != __LONG_LONG_MAX__ - 27ULL; i -= 3 - 2)
    a[i + 26LL - __LONG_LONG_MAX__] -= 4;
}

__attribute__((noinline, noclone)) void
N(f23) (long long n1, long long n2)
{
  OMPTGT
#pragma omp F S
  for (long long i = n1 + 23; i != n2 - 25; --i)
    a[i + 48] += 7;
}

__attribute__((noinline, noclone)) void
N(f24) (void)
{
  unsigned int i;
  OMPTGT
#pragma omp F S
  for (i = 30; i != 30; i += 1)
    a[i] += 10;
}

__attribute__((noinline, noclone)) void
N(f25) (int n11, int n12, int n21, int n22, int n31, int n32,
	int s2)
{
  SC int v1, v2, v3;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = n11; v1 != n12; v1 += 17 - 19 + 3)
    for (v2 = n21; v2 < n22; v2 += s2)
      for (v3 = n31; v3 != n32; ++v3)
	b[v1][v2][v3] += 2.5;
}

__attribute__((noinline, noclone)) void
N(f26) (int n11, int n12, int n21, int n22, long long n31, long long n32,
	int s2)
{
  SC int v1, v2;
  SC long long v3;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = n11; v1 != n12; v1 += -1)
    for (v2 = n21; v2 > n22; v2 += s2)
      for (v3 = n31; v3 != n32; v3 --)
	b[v1][v2 / 2][v3] -= 4.5;
}

__attribute__((noinline, noclone)) void
N(f27) (void)
{
  SC unsigned int v1, v3;
  SC unsigned long long v2;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = 0; v1 < 20; v1 += 2)
    for (v2 = __LONG_LONG_MAX__ + 11ULL;
	 v2 != __LONG_LONG_MAX__ - 4ULL; -- v2)
      for (v3 = 10; v3 != 0; v3--)
	b[v1 >> 1][v2 - __LONG_LONG_MAX__ + 3][v3 - 1] += 5.5;
}

__attribute__((noinline, noclone)) void
N(f28) (void)
{
  SC long long v1, v2, v3;
  OMPTGT
#pragma omp F S collapse(3)
  for (v1 = 0; v1 != 20; v1 -= 17 - 18)
    for (v2 = 30; v2 < 20; v2++)
      for (v3 = 10; v3 < 0; v3--)
	b[v1][v2][v3] += 5.5;
}

__attribute__((noinline, noclone)) void
N(f29) (void)
{
  int i;
  OMPTGT
#pragma omp F S
  for (i = 20; i != 20; i++)
    {
      a[i] += 2;
      noreturn ();
      a[i] -= 4;
    }
}

__attribute__((noinline, noclone)) void
N(f30) (void)
{
  SC int i;
  OMPTGT
#pragma omp F S collapse(3)
  for (i = 0; i != 10; i++)
    for (int j = 10; j < 8; j++)
      for (long k = -10; k != 10; k++)
	{
	  b[i][j][k] += 4;
	  noreturn ();
	  b[i][j][k] -= 8;
	}
}

__attribute__((noinline, noclone)) void
N(f31) (int n)
{
  int i;
  OMPTGT
#pragma omp F S
  for (i = 20; i != n; i++)
    {
      a[i] += 8;
      noreturn ();
      a[i] -= 16;
    }
}

__attribute__((noinline, noclone)) void
N(f32) (int n)
{
  SC int i;
  OMPTGT
#pragma omp F S collapse(3)
  for (i = 0; i != 10; i++)
    for (int j = n; j != 12; j++)
      for (long k = -10; k != 10; k++)
	{
	  b[i][j][k] += 16;
	  noreturn ();
	  b[i][j][k] -= 32;
	}
}

__attribute__((noinline, noclone)) void
N(f33) (void)
{
  int *i;
  OMPTGT
#pragma omp F S
  for (i = a; i != &a[1500]; i++)
    i[0] += 2;
}

__attribute__((noinline, noclone)) void
N(f34) (void)
{
  SC float *i;
  OMPTGT
#pragma omp F S collapse(3)
  for (i = &b[0][0][0]; i != &b[0][0][10]; i++)
    for (float *j = &b[0][15][0]; j > &b[0][0][0]; j -= 10)
      for (float *k = &b[0][0][10]; k != &b[0][0][0]; --k)
	b[i - &b[0][0][0]][(j - &b[0][0][0]) / 10 - 1][(k - &b[0][0][0]) - 1]
	  -= 3.5;
}

__attribute__((noinline, noclone)) int
N(test) (void)
{
  int i, j, k;
  for (i = 0; i < 1500; i++)
    a[i] = i - 25;
  OMPTO (a);
  OMPTEAMS
  N(f20) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 23)
      return 1;
  OMPTEAMS
  N(f21) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 25)
      return 1;
  OMPTEAMS
  N(f22) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 29)
      return 1;
  OMPTEAMS
  N(f23) (1500LL - 1 - 23 - 48, -1LL + 25 - 48);
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  OMPTEAMS
  N(f24) ();
  OMPFROM (a);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	b[i][j][k] = i - 2.5 + 1.5 * j - 1.5 * k;
  OMPTO (b);
  OMPTEAMS
  N(f25) (0, 10, 0, 15, 0, 10, 1);
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f25) (0, 10, 30, 15, 0, 10, 5);
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f26) (9, -1, 29, 0, 9, -1, -2);
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i - 4.5 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f27) ();
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.0 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f28) ();
  OMPFROM (b);
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.0 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f29) ();
  OMPTEAMS
  N(f30) ();
  OMPTEAMS
  N(f31) (20);
  OMPTEAMS
  N(f32) (12);
  OMPFROM (a);
  OMPFROM (b);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 22)
      return 1;
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i + 1.0 + 1.5 * j - 1.5 * k)
	  return 1;
  OMPTEAMS
  N(f33) ();
  OMPTEAMS
  N(f34) ();
  OMPFROM (a);
  OMPFROM (b);
  for (i = 0; i < 1500; i++)
    if (a[i] != i - 20)
      return 1;
  for (i = 0; i < 10; i++)
    for (j = 0; j < 15; j++)
      for (k = 0; k < 10; k++)
	if (b[i][j][k] != i - 2.5 + 1.5 * j - 1.5 * k)
	  return 1;
  return 0;
}
#endif
