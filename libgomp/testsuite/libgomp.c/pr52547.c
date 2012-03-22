/* PR middle-end/52547 */
/* { dg-do run } */

extern void abort (void);

__attribute__((noinline, noclone)) int
baz (int *x, int (*fn) (int *))
{
  return fn (x);
}

__attribute__((noinline, noclone)) int
foo (int x, int *y)
{
  int i, e = 0;
#pragma omp parallel for reduction(|:e)
  for (i = 0; i < x; ++i)
    {
      __label__ lab;
      int bar (int *z) { return z - y; }
      if (baz (&y[i], bar) != i)
	e |= 1;
    }
  return e;
}

int
main ()
{
  int a[100], i;
  for (i = 0; i < 100; i++)
    a[i] = i;
  if (foo (100, a))
    abort ();
  return 0;
}
