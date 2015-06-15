/* { dg-do run } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-additional-options "-std=c99" } */

int
main ()
{
  int b, c = 0;
  b = 7;
  #pragma omp simd linear(b:2) reduction(+:c)
  for (int i = 0; i < 64; i++)
    {
      c = c + (b != 7 + 2 * i);
      b = b + 2;
    }
  if (c || b != 7 + 64 * 2)
    __builtin_abort ();
  b = 7;
  #pragma omp simd linear(b:3) reduction(+:c)
  for (int i = 0; i < 64; i += 4)
    {
      c = c + (b != 7 + i / 4 * 3);
      b = b + 3;
    }
  if (c || b != 7 + 16 * 3)
    __builtin_abort ();
  b = 7;
  #pragma omp simd collapse (2) linear(b:2) reduction(+:c)
  for (int i = 0; i < 8; i++)
    for (int j = 0; j < 8; j++)
      {
	c = c + (b != 7 + 2 * j + 2 * 8 * i);
	b = b + 2;
      }
  if (c || b != 7 + 64 * 2)
    __builtin_abort ();
  b = 7;
  #pragma omp parallel for simd schedule (static, 4) linear(b:2) reduction(+:c)
  for (int i = 0; i < 64; i++)
    {
      c = c + (b != 7 + 2 * i);
      b = b + 2;
    }
  if (c || b != 7 + 64 * 2)
    __builtin_abort ();
  b = 7;
  #pragma omp parallel for simd schedule (static, 4) linear(b:3) reduction(+:c)
  for (int i = 0; i < 64; i += 4)
    {
      c = c + (b != 7 + i / 4 * 3);
      b = b + 3;
    }
  if (c || b != 7 + 16 * 3)
    __builtin_abort ();
  b = 7;
  #pragma omp parallel for simd collapse (2) schedule (static, 4) linear(b:2) reduction(+:c)
  for (int i = 0; i < 8; i++)
    for (int j = 0; j < 8; j++)
      {
	c = c + (b != 7 + 2 * j + 2 * 8 * i);
	b = b + 2;
      }
  if (c || b != 7 + 64 * 2)
    __builtin_abort ();
  return 0;
}
