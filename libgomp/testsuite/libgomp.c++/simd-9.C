// { dg-do run }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

extern "C" void abort ();
int a[1024] __attribute__((aligned (32))) = { 1 };
#pragma omp declare reduction (foo:int:omp_out += omp_in) \
		    initializer (omp_priv = 0)

__attribute__((noinline, noclone)) void
foo (int &u, int &v)
{
  int i;
  #pragma omp simd aligned(a : 32) reduction(foo:u) reduction(+:v)
  for (i = 0; i < 1024; i++)
    {
      int x = a[i];
      u += x;
      v += x;
    }
}

__attribute__((noinline, noclone)) void
bar (int &u, int &v)
{
  int i;
  #pragma omp simd aligned(a : 32) reduction(foo:u) reduction(+:v) \
		   safelen(1)
  for (i = 0; i < 1024; i++)
    {
      int x = a[i];
      u += x;
      v += x;
    }
}

int
main ()
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (i & 31) + (i / 128);
  int u = 0, v = 0;
  foo (u, v);
  if (u != 19456 || v != 19456)
    abort ();
  u = 0; v = 0;
  bar (u, v);
  if (u != 19456 || v != 19456)
    abort ();
}
