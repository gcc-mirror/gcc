// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

extern "C" void abort ();
int a[1024] __attribute__((aligned (32))) = { 1 };
int b[1024] __attribute__((aligned (32))) = { 1 };
unsigned char c[1024] __attribute__((aligned (32))) = { 1 };
int k, m;
__UINTPTR_TYPE__ u, u2, u3;

__attribute__((noinline, noclone)) int
foo (int *p)
{
  int i, s = 0, s2 = 0, t, t2;
  #pragma omp simd aligned(a, b, p : 32) linear(k: m + 1) reduction(+:s) \
		   lastprivate (t2)
  for (i = 0; i < 512; i++)
    {
      a[i] *= p[i];
      t2 = k + p[i];
      k += m + 1;
      s += p[i] + k;
      c[i]++;
    }
  #pragma omp simd aligned(a, b, p : 32) linear(k: m + 1) reduction(+:s2) \
		   lastprivate (t, u, u2, u3)
  for (i = 512; i < 1024; i++)
    {
      a[i] *= p[i];
      k += m + 1;
      t = k + p[i];
      u = (__UINTPTR_TYPE__) &k;
      u2 = (__UINTPTR_TYPE__) &s2;
      u3 = (__UINTPTR_TYPE__) &t;
      s2 += t;
      c[i]++;
    }
  return s + s2 + t + t2;
}

__attribute__((noinline, noclone)) long int
bar (int *p, long int n, long int o)
{
  long int i, s = 0, s2 = 0, t, t2;
  #pragma omp simd aligned(a, b, p : 32) linear(k: m + 1) reduction(+:s) \
		   lastprivate (t2)
  for (i = 0; i < n; i++)
    {
      a[i] *= p[i];
      t2 = k + p[i];
      k += m + 1;
      s += p[i] + k;
      c[i]++;
    }
  #pragma omp simd aligned(a, b, p : 32) linear(k: m + 1) reduction(+:s2) \
		   lastprivate (t, u, u2, u3)
  for (i = n; i < o; i++)
    {
      a[i] *= p[i];
      k += m + 1;
      t = k + p[i];
      u = (__UINTPTR_TYPE__) &k;
      u2 = (__UINTPTR_TYPE__) &s2;
      u3 = (__UINTPTR_TYPE__) &t;
      s2 += t;
      c[i]++;
    }
  return s + s2 + t + t2;
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
      c[i] = (unsigned char) i;
    }
  int s = foo (b);
  for (i = 0; i < 1024; i++)
    {
      if (b[i] != (i - 51) % 39
	  || a[i] != (i - 512) * b[i]
	  || c[i] != (unsigned char) (i + 1))
	abort ();
      a[i] = i - 512;
    }
  if (k != 4 + 3 * 1024
      || s != 1596127 + (4 + 3 * 511 + b[511]) + (4 + 3 * 1024 + b[1023]))
    abort ();
  k = 4;
  s = bar (b, 512, 1024);
  for (i = 0; i < 1024; i++)
    {
      if (b[i] != (i - 51) % 39
	  || a[i] != (i - 512) * b[i]
	  || c[i] != (unsigned char) (i + 2))
	abort ();
      a[i] = i - 512;
    }
  if (k != 4 + 3 * 1024
      || s != 1596127 + (4 + 3 * 511 + b[511]) + (4 + 3 * 1024 + b[1023]))
    abort ();
  k = 4;
  s = bar (b, 511, 1021);
  for (i = 0; i < 1021; i++)
    {
      if (b[i] != (i - 51) % 39
	  || a[i] != (i - 512) * b[i]
	  || c[i] != (unsigned char) (i + 3))
	abort ();
      a[i] = i - 512;
    }
  for (i = 1021; i < 1024; i++)
    if (b[i] != (i - 51) % 39
	|| a[i] != i - 512
	|| c[i] != (unsigned char) (i + 2))
      abort ();
  if (k != 4 + 3 * 1021
      || s != 1586803 + (4 + 3 * 510 + b[510]) + (4 + 3 * 1021 + b[1020]))
    abort ();
#endif
  return 0;
}
