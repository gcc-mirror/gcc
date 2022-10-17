/* { dg-do run } */
/* { dg-additional-options "-O2 -fvect-cost-model=cheap -fdump-tree-vect-details" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" { target avx_runtime } } } */

int v, x;

__attribute__((noipa)) void
foo (int *a)
{
  #pragma omp for simd lastprivate (conditional: x) schedule(simd: static)
  for (int i = 0; i < 128; i++)
    if (a[i])
      x = a[i];
}

__attribute__((noipa)) void
bar (int *a, int *b)
{
  #pragma omp for simd lastprivate (conditional: x, v) schedule(static, 16)
  for (int i = 16; i < 128; ++i)
    {
      if (a[i])
	x = a[i];
      if (b[i])
	v = b[i] + 10;
    }
}

__attribute__((noipa)) void
baz (int *a)
{
  #pragma omp for simd lastprivate (conditional: x) schedule(simd: dynamic, 16)
  for (int i = 0; i < 128; i++)
    if (a[i])
      x = a[i] + 5;
}

int
main ()
{
  int a[128], b[128], i;
  for (i = 0; i < 128; i++)
    {
      a[i] = ((i % 11) == 2) ? i + 10 : 0;
      asm volatile ("" : "+g" (i));
      b[i] = ((i % 13) == 5) ? i * 2 : 0;
    }
  #pragma omp parallel
  foo (a);
  if (x != 133)
    __builtin_abort ();
  x = -3;
  #pragma omp parallel
  bar (b, a);
  if (x != 244 || v != 143)
    __builtin_abort ();
  #pragma omp parallel
  baz (b);
  if (x != 249)
    __builtin_abort ();
  return 0;
}
