/* { dg-do run } */
/* { dg-options "-O2 -std=c99" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

int u[1024], v[1024], w[1024], m;

__attribute__((noinline, noclone)) void
f1 (long a, long b)
{
  #pragma omp taskloop simd default(none) shared(u, v, w) nogroup
  for (long d = a; d < b; d++)
    u[d] = v[d] + w[d];
}

__attribute__((noinline, noclone)) int
f2 (long a, long b, long c)
{
  int d, e;
  #pragma omp taskloop simd default(none) shared(u, v, w) linear(d:1) linear(c:5) lastprivate(e)
  for (d = a; d < b; d++)
    {
      u[d] = v[d] + w[d];
      c = c + 5;
      e = c + 9;
    }
  return d + c + e;
}

__attribute__((noinline, noclone)) int
f3 (long a, long b)
{
  int d;
  #pragma omp taskloop simd default(none) shared(u, v, w)
  for (d = a; d < b; d++)
    {
      int *p = &d;
      u[d] = v[d] + w[d];
    }
  return d;
}

__attribute__((noinline, noclone)) int
f4 (long a, long b, long c, long d)
{
  int e, f, g;
  #pragma omp taskloop simd default(none) shared(u, v, w) collapse(2) lastprivate(g)
  for (e = a; e < b; e++)
    for (f = c; f < d; f++)
      {
	int *p = &e;
	int *q = &f;
	int r = 32 * e + f;
	u[r] = v[r] + w[r];
	g = r;
      }
  return e + f + g;
}

__attribute__((noinline, noclone)) int
f5 (long a, long b, long c, long d)
{
  int e, f;
  #pragma omp taskloop simd default(none) shared(u, v, w) collapse(2)
  for (e = a; e < b; e++)
    for (f = c; f < d; f++)
      {
	int r = 32 * e + f;
	u[r] = v[r] + w[r];
      }
  return e + f;
}

int
main ()
{
  int i;
  for (i = 0; i < 1024; i++)
    {
      v[i] = i;
      w[i] = i + 1;
    }
  #pragma omp parallel
    #pragma omp single
      f1 (0, 1024);
  for (i = 0; i < 1024; i++)
    if (u[i] != 2 * i + 1)
      __builtin_abort ();
    else
      {
	v[i] = 1024 - i;
	w[i] = 512 - i;
      }
  #pragma omp parallel
    #pragma omp single
      m = f2 (2, 1022, 17);
  for (i = 0; i < 1024; i++)
    if ((i < 2 || i >= 1022) ? u[i] != 2 * i + 1 : u[i] != 1536 - 2 * i)
      __builtin_abort ();
    else
      {
	v[i] = i;
	w[i] = i + 1;
      }
  if (m != 1022 + 2 * (1020 * 5 + 17) + 9)
    __builtin_abort ();
  #pragma omp parallel
    #pragma omp single
      m = f3 (0, 1024);
  for (i = 0; i < 1024; i++)
    if (u[i] != 2 * i + 1)
      __builtin_abort ();
    else
      {
	v[i] = 1024 - i;
	w[i] = 512 - i;
      }
  if (m != 1024)
    __builtin_abort ();
  #pragma omp parallel
    #pragma omp single
      m = f4 (0, 32, 0, 32);
  for (i = 0; i < 1024; i++)
    if (u[i] != 1536 - 2 * i)
      __builtin_abort ();
    else
      {
	v[i] = i;
	w[i] = i + 1;
      }
  if (m != 32 + 32 + 1023)
    __builtin_abort ();
  #pragma omp parallel
    #pragma omp single
      m = f5 (0, 32, 0, 32);
  for (i = 0; i < 1024; i++)
    if (u[i] != 2 * i + 1)
      __builtin_abort ();
    else
      {
	v[i] = 1024 - i;
	w[i] = 512 - i;
      }
  if (m != 32 + 32)
    __builtin_abort ();
  return 0;
}
