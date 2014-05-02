/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

int s = 0, i, j, u;

void
foo ()
{
  #pragma omp for simd schedule(static, 32) reduction(+:s) lastprivate(u) collapse(2)
  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      {
	s++;
	u = i + j;
      }
  if (i != 16 || j != 16 || s != 256 || u != 30)
    __builtin_abort ();
}

int
main ()
{
  foo ();
  return 0;
}
