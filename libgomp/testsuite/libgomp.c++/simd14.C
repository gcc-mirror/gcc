// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

int a[1024];
short b[2048];

static inline void
bar (int &x, unsigned long long &y, short *&z)
{
  a[x] = x + y + *z;
  x++;
  y += 17;
  z += 2;
}

__attribute__((noinline, noclone)) int
foo (unsigned long long &s, short *&t)
{
  int i, j = 0;
  int &r = j;
#pragma omp parallel for simd linear(r) linear(s:17ULL) linear(t:2)
  for (i = 0; i < 1024; i++)
    bar (r, s, t);
  return j;
}

int
main ()
{
  int i;
  for (i = 0; i < 2048; i++)
    b[i] = 3 * i;
  unsigned long long s = 12;
  short *t = b;
  int j = foo (s, t);
  for (i = 0; i < 1024; i++)
    if (a[i] != 12 + 24 * i)
      __builtin_abort ();
  if (j != 1024 || s != 12 + 1024 * 17ULL || t != &b[2048])
    __builtin_abort ();
}
