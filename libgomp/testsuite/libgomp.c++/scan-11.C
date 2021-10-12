// { dg-require-effective-target size32plus }
// { dg-additional-options "-O2 -fopenmp -fdump-tree-vect-details" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-final { scan-tree-dump-times "vectorized \[2-6] loops" 2 "vect" { target sse2_runtime } } }

extern "C" void abort ();
int r, a[1024], b[1024], q;

#pragma omp declare reduction (foo: int: omp_out += omp_in) initializer (omp_priv = 0)

__attribute__((noipa)) void
foo (int *a, int *b, int &r)
{
  #pragma omp for simd reduction (inscan, foo:r)
  for (int i = 0; i < 1024; i++)
    {
      r += a[i];
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) int
bar (void)
{
  int &s = q;
  q = 0;
  #pragma omp parallel
  #pragma omp for simd reduction (inscan, foo:s) if (0)
  for (int i = 0; i < 1024; i++)
    {
      s += 2 * a[i];
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

__attribute__((noipa)) void
baz (int *a, int *b, int &r)
{
  #pragma omp parallel for simd simdlen (1) reduction (inscan, foo:r)
  for (int i = 0; i < 1024; i++)
    {
      r += a[i];
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) int
qux (void)
{
  int &s = q;
  q = 0;
  #pragma omp parallel for simd reduction (inscan, foo:s)
  for (int i = 0; i < 1024; i++)
    {
      s += 2 * a[i];
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

int
main ()
{
  int s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      a[i] = i;
      b[i] = -1;
      asm ("" : "+g" (i));
    }
  #pragma omp parallel
  foo (a, b, r);
  if (r != 1024 * 1023 / 2)
    abort ();
  for (int i = 0; i < 1024; ++i)
    {
      s += i;
      if (b[i] != s)
	abort ();
      else
	b[i] = 25;
    }
  if (bar () != 1024 * 1023)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      s += 2 * i;
      if (b[i] != s)
	abort ();
      else
	b[i] = -1;
    }
  r = 0;
  baz (a, b, r);
  if (r != 1024 * 1023 / 2)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      s += i;
      if (b[i] != s)
	abort ();
      else
	b[i] = -25;
    }
  if (qux () != 1024 * 1023)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      s += 2 * i;
      if (b[i] != s)
	abort ();
    }
  return 0;
}
