/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

int
main ()
{
  int k = 0, i, s = 0;
  #pragma omp parallel for simd linear(k : 3) reduction(+: s) schedule (static, 16)
  for (i = 0; i < 128; i++)
    {
      k = k + 3;
      s = s + k;
    }
  if (s != 128 * 129 / 2 * 3) __builtin_abort ();
  return 0;
}
