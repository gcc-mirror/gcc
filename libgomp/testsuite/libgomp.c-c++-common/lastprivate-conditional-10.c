/* { dg-do run } */

int v, x;

__attribute__((noipa)) int
foo (int *a)
{
  #pragma omp parallel for simd lastprivate (conditional: x) schedule(simd: static) if (simd: 0)
  for (int i = 0; i < 128; i++)
    if (a[i])
      x = a[i];
  return x;
}

__attribute__((noipa)) int
bar (int *a, int *b)
{
  #pragma omp parallel
  #pragma omp for simd lastprivate (conditional: x, v) schedule(static, 16) simdlen (1)
  for (int i = 16; i < 128; ++i)
    {
      if (a[i])
	x = a[i];
      if (b[i])
	v = b[i] + 10;
    }
  return x;
}

__attribute__((noipa)) int
baz (int *a)
{
  #pragma omp parallel for simd if (simd: 0) lastprivate (conditional: x) schedule(simd: dynamic, 16)
  for (int i = 0; i < 128; i++)
    if (a[i])
      x = a[i] + 5;
  return x;
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
  if (foo (a) != 133)
    __builtin_abort ();
  if (bar (b, a) != 244 || v != 143)
    __builtin_abort ();
  if (baz (b) != 249)
    __builtin_abort ();
  return 0;
}
