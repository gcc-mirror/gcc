/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

extern void abort ();
int a[32][32] __attribute__((aligned (32))) = { { 1 } };
struct S { int s; };
#pragma omp declare reduction (+:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:int:omp_out += omp_in)

__attribute__((noinline, noclone)) int
foo (void)
{
  int i, j, u = 0;
  struct S s, t;
  s.s = 0; t.s = 0;
  #pragma omp simd aligned(a : 32) reduction(+:s) reduction(foo:t, u) collapse(2)
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; j++)
      {
	int x = a[i][j];
	s.s += x;
	t.s += x;
	u += x;
      }
  if (t.s != s.s || u != s.s)
    abort ();
  return s.s;
}

int
main ()
{
  int i, j;
  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; j++)
      a[i][j] = j + (i / 4);
  int s = foo ();
  if (s != 19456)
    abort ();
  return 0;
}
