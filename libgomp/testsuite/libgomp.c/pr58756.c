/* PR libgomp/58756 */
/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

extern void abort (void);
int d[32 * 32];

__attribute__((noinline, noclone)) int
foo (int a, int b)
{
  int j, c = 0;
  #pragma omp parallel for reduction(+: c)
    for (j = 0; j < a; j += 32)
      {
	int l;
	#pragma omp simd reduction(+: c) safelen(1)
	  for (l = 0; l < b; ++l)
	    c += d[j + l];
      }
  return c;
}

__attribute__((noinline, noclone)) int
bar (int a)
{
  int j, c = 0;
  #pragma omp parallel for simd reduction(+: c) safelen(1)
    for (j = 0; j < a; ++j)
      c += d[j];
  return c;
}

__attribute__((noinline)) static int
baz (int a)
{
  int j, c = 0;
  #pragma omp simd reduction(+: c) safelen(1)
    for (j = 0; j < a; ++j)
      c += d[j];
  return c;
}

int
main ()
{
  int i;
  for (i = 0; i < 32 * 32; i++)
    d[i] = (i & 31);
  if (foo (32 * 32, 32) != (31 * 32 / 2) * 32)
    abort ();
  if (bar (32 * 32) != (31 * 32 / 2) * 32)
    abort ();
  if (baz (32 * 32) != (31 * 32 / 2) * 32)
    abort ();
  return 0;
}
