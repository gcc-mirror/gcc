// { dg-do run }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

extern "C" void abort ();
int a[1024] __attribute__((aligned (32))) = { 1 };
int b[1024] __attribute__((aligned (32))) = { 1 };
int k, m;
struct U { U (); ~U (); int u; };
struct V
{
  V () : v (8) {}
  ~V ()
  {
    if (v > 38 + 4 + 3 * 1024 + 1)
      abort ();
  }
  V &operator= (const V &x) { v = x.v + 1; return *this; }
  int v;
};

__attribute__((noinline, noclone))
U::U () : u (6)
{
}

__attribute__((noinline, noclone))
U::~U ()
{
  if (u > 38 + 4 + 3 * 1023)
    abort ();
}

__attribute__((noinline, noclone)) int
foo (int *p)
{
  int i, s = 0;
  U u;
  V v;
  #pragma omp simd aligned(a, p : 32) linear(k: m + 1) \
		   reduction(+:s) lastprivate(u, v)
  for (i = 0; i < 1024; i++)
    {
      a[i] *= p[i];
      u.u = p[i] + k;
      k += m + 1;
      v.v = p[i] + k;
      s += p[i] + k;
    }
  if (u.u != 36 + 4 + 3 * 1023 || v.v != 36 + 4 + 3 * 1024 + 1)
    abort ();
  return s;
}

int
main ()
{
#if __SIZEOF_INT__ >= 4
  int i;
  k = 4;
  m = 2;
  for (i = 0; i < 1024; i++)
    {
      a[i] = i - 512;
      b[i] = (i - 51) % 39;
    }
  int s = foo (b);
  for (i = 0; i < 1024; i++)
    {
      if (b[i] != (i - 51) % 39
	  || a[i] != (i - 512) * b[i])
	abort ();
    }
  if (k != 4 + 3 * 1024 || s != 1596127)
    abort ();
#endif
  return 0;
}
