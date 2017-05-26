/* PR middle-end/80809 */
/* { dg-do run } */

__attribute__((noinline, noclone)) void
foo (int x)
{
  int i, v[x], w[16];
  for (i = 0; i < x; i++)
    v[i] = i;
  for (i = 0; i < 16; i++)
    w[i] = 0;
#pragma omp parallel
#pragma omp single
  for (i = 0; i < 16; i++)
#pragma omp task firstprivate (v)
    {
      int j;
      for (j = 0; j < x; j++)
	v[j] += i;
      for (j = 0; j < x; j++)
	w[i] += v[j];
    }
  for (i = 0; i < 16; i++)
    if (w[i] != (x - 1) * x / 2 + x * i)
      __builtin_abort ();
}

int
main ()
{
  foo (4);
  foo (27);
  foo (196);
  return 0;
}
