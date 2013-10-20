/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

extern void abort ();
int a[1024] __attribute__((aligned (32))) = { 1 };
struct S { int s; };
#pragma omp declare reduction (+:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:int:omp_out += omp_in)

__attribute__((noinline, noclone)) int
foo (void)
{
  int i, u = 0, q = 0;
  struct S s, t;
  s.s = 0; t.s = 0;
  #pragma omp simd aligned(a : 32) reduction(+:s, q) reduction(foo:t, u) \
	      safelen(1)
  for (i = 0; i < 1024; i++)
    {
      int x = a[i];
      s.s += x;
      t.s += x;
      u += x;
      q++;
    }
  if (t.s != s.s || u != s.s || q != 1024)
    abort ();
  return s.s;
}

int
main ()
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (i & 31) + (i / 128);
  int s = foo ();
  if (s != 19456)
    abort ();
  return 0;
}
