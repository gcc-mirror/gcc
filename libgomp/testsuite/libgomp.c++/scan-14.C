// { dg-require-effective-target size32plus }
// { dg-additional-options "-O2 -fopenmp -fdump-tree-vect-details" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-final { scan-tree-dump-times "vectorized \[2-6] loops" 2 "vect" { target sse2_runtime } } }

extern "C" void abort ();
int r, a[1024], b[1024], q;

template <typename T, typename U>
__attribute__((noipa)) void
foo (T a, T b, U r)
{
  #pragma omp for simd if (0) reduction (inscan, +:r)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r += a[i];
    }
}

template <typename T>
__attribute__((noipa)) T
bar ()
{
  T &s = q;
  q = 0;
  #pragma omp parallel
  #pragma omp for simd reduction (inscan, +:s) simdlen(1)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s += 2 * a[i];
    }
  return s;
}

template <typename T>
__attribute__((noipa)) void
baz (T *a, T *b, T &r)
{
  #pragma omp parallel for simd reduction (inscan, +:r)
  for (T i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r += a[i];
    }
}

template <typename T>
__attribute__((noipa)) int
qux ()
{
  T s = q;
  q = 0;
  #pragma omp parallel for simd reduction (inscan, +:s)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s += 2 * a[i];
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
  foo<int *, int &> (a, b, r);
  if (r != 1024 * 1023 / 2)
    abort ();
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = 25;
      s += i;
    }
  if (bar<int> () != 1024 * 1023)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = -1;
      s += 2 * i;
    }
  r = 0;
  baz<int> (a, b, r);
  if (r != 1024 * 1023 / 2)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      else
	b[i] = -25;
      s += i;
    }
  if (qux<int &> () != 1024 * 1023)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s)
	abort ();
      s += 2 * i;
    }
}
