/* PR middle-end/80809 */
/* { dg-do run } */

__attribute__((noinline, noclone)) void
foo (int x)
{
  int i, j, v[x], *w[16];
  for (i = 0; i < x; i++)
    v[i] = i;
#pragma omp parallel
#pragma omp single
  for (i = 0; i < 16; i++)
  /* Make sure v is implicitly determined shared in task, because it
     is shared on the parallel.  */
#pragma omp task private (j)
    w[i] = v;
  for (i = 0; i < 16; i++)
    if (w[i] != v)
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
