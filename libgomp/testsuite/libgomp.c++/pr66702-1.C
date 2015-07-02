// PR middle-end/66702
// { dg-do run { target vect_simd_clones } }
// { dg-options "-O2" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

void
bar (int &a, int &b, int *&c, int &d)
{
  volatile int x;
  int *volatile y;
  x = a; a = x;
  x = b; b = x;
  y = c; c = y;
  x = d; d = x;
}

void (*volatile barp) (int &, int &, int *&, int &) = bar;

#pragma omp declare simd uniform(b, c) linear(d:2) aligned(c:32) notinbranch
int
foo (int a, int b, int *c, int d)
{
  a++;
  b++;
  c += 8;
  d += 2;
  barp (a, b, c, d);
  return a + b + *c + d;
}

volatile int e = 5;
int c[64] __attribute__((aligned (32)));

int
main ()
{
  int d = 7, r = 0;
  int b = e;
  for (int i = 0; i < 64; i++)
    c[i] = i;
  #pragma omp simd reduction(+:r) linear(d:2)
  for (int i = 0; i < 64; i++)
    {
      r += foo (i, b, c, d);
      d += 2;
    }
  if (r != 7584)
    __builtin_abort ();
}
