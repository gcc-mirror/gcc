/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

int s = 0, i, u;

void
foo ()
{
  #pragma omp for simd schedule(static, 32) reduction(+:s) lastprivate(u)
  for (i = 0; i < 128; i++)
    {
      s++;
      u = i;
    }
  if (i != 128 || s != 128 || u != 127)
    __builtin_abort ();
}

int
main ()
{
  foo ();
  return 0;
}
