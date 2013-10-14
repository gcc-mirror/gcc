// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

extern "C" void abort ();
int a[1024] __attribute__((aligned (32))) = { 1 };
struct S
{
  int s;
  S () : s (0) {}
  S (int x) : s (x) {}
  ~S () {}
};
#pragma omp declare reduction (+:S:omp_out.s += omp_in.s) \
		    initializer (omp_priv (0))
#pragma omp declare reduction (foo:S:omp_out.s += omp_in.s) \
		    initializer (omp_priv (0))
#pragma omp declare reduction (foo:int:omp_out += omp_in) \
		    initializer (omp_priv = 0)

__attribute__((noinline, noclone)) S
foo (S s)
{
  int i, v = 0, &u = v;
  S t;
  #pragma omp simd aligned(a : 32) reduction(+:s) reduction(foo:t, u)
  for (i = 0; i < 1024; i++)
    {
      int x = a[i];
      s.s += x;
      t.s += x;
      u += x;
    }
  if (t.s != s.s || u != s.s)
    abort ();
  return t;
}

__attribute__((noinline, noclone)) int
bar (S &s, S &t)
{
  int i, v = 0, &u = v;
  #pragma omp simd aligned(a : 32) reduction(+:s) reduction(foo:t, u)
  for (i = 0; i < 1024; i++)
    {
      int x = a[i];
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
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (i & 31) + (i / 128);
  S q;
  int s = foo (q).s;
  if (s != 19456)
    abort ();
  S r, v;
  if (bar (r, v) != s)
    abort ();
}
