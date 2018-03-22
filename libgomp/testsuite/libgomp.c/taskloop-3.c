/* { dg-do run } */
/* { dg-options "-O2 -std=c99" } */

int g;
int a[1024];

__attribute__((noinline, noclone)) int
f1 (int x)
{
  #pragma omp taskloop firstprivate (x) lastprivate (x)
  for (int i = 0; i < 64; i++)
    {
      if (x != 74)
	__builtin_abort ();
      if (i == 63)
	x = i + 4;
    }
  return x;
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  #pragma omp taskloop firstprivate (g) lastprivate (g) nogroup
  for (int i = 0; i < 64; i++)
    {
      if (g != 77)
	__builtin_abort ();
      if (i == 63)
	g = i + 9;
    }
}

__attribute__((noinline, noclone)) long long
f3 (long long a, long long b, long long c)
{
  long long i;
  int l;
  #pragma omp taskloop default (none) lastprivate (i, l)
  for (i = a; i < b; i += c)
    l = i;
  return l * 7 + i;
}

__attribute__((noinline, noclone)) long long
f4 (long long a, long long b, long long c, long long d,
    long long e, long long f, int k)
{
  long long i, j;
  int l;
  #pragma omp taskloop default (none) collapse(2) \
	      firstprivate (k) lastprivate (i, j, k, l)
  for (i = a; i < b; i += e)
    for (j = c; j < d; j += f)
      {
	if (k != 73)
	  __builtin_abort ();
	if (i == 31 && j == 46)
	  k = i;
	l = j;
      }
  return i + 5 * j + 11 * k + 17 * l;
}

int
main ()
{
  #pragma omp parallel
    #pragma omp single
      {
	if (f1 (74) != 63 + 4)
	  __builtin_abort ();
	g = 77;
	f2 ();
	#pragma omp taskwait
	if (g != 63 + 9)
	  __builtin_abort ();
	if (f3 (7, 12, 2) != 11 * 7 + 13)
	  __builtin_abort ();
	if (f4 (0, 32, 16, 48, 1, 2, 73) != 32 + 5 * 48 + 11 * 31 + 17 * 46)
	  __builtin_abort ();
      }
  return 0;
}
