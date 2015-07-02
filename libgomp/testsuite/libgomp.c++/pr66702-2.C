// PR middle-end/66702
// { dg-do run { target vect_simd_clones } }
// { dg-options "-O2" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

struct S { int s1, s2; };
struct T { T (); ~T (); int t; };

T::T () : t(0) {}
T::~T () {}

#pragma omp declare simd uniform(b, c) notinbranch
__attribute__((noinline)) int
foo (int a, S b, T c)
{
  a++;
  b.s1++;
  b.s2++;
  c.t++;
  return a + b.s1 + b.s2 + c.t;
}

int
main ()
{
  int r = 0;
  S s = { 2, 3 };
  T t;
  #pragma omp simd reduction(+:r)
  for (int i = 0; i < 64; i++)
    r += foo (i, s, t);
  if (r != 2592)
    __builtin_abort ();
}
