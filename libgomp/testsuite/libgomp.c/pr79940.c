/* PR c/79940 */

int
main ()
{
  int i, j, l, m;
  int a[10000], b[10000], c[10000];
  for (i = 0; i < 10000; i++)
    {
      a[i] = i;
      b[i] = i & 31;
    }
#pragma omp parallel shared(a, b, c)
#pragma omp single
#pragma omp taskloop shared(a, b, c)
  for (i = 0; i < 10000; i++)
    c[i] = a[i] + b[i];
#pragma omp parallel
#pragma omp single
  {
    #pragma omp taskloop shared(a, b, c) lastprivate (i)
    for (i = 0; i < 10000; i++)
      c[i] += a[i] + b[i];
    l = i;
  }
#pragma omp parallel
#pragma omp single
#pragma omp taskloop shared(a, b, c) collapse(2)
  for (i = 0; i < 100; i++)
    for (j = 0; j < 100; j++)
      c[i * 100 + j] += a[i * 100 + j] + b[i * 100 + j];
#pragma omp parallel
#pragma omp single
  {
    #pragma omp taskloop shared(a, b, c) lastprivate (i, j)
    for (i = 0; i < 100; i++)
      for (j = 0; j < 100; j++)
	c[i * 100 + j] += a[i * 100 + j] + b[i * 100 + j];
    m = i * 100 + j;
  }
  for (i = 0; i < 10000; i++)
    if (a[i] != i || b[i] != (i & 31) || c[i] != 4 * i + 4 * (i & 31))
      __builtin_abort ();
  if (l != 10000 || m != 10100)
    __builtin_abort ();
  return 0;
}
