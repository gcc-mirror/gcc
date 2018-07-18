/* PR middle-end/80853 */
/* { dg-do run } */

__attribute__((noinline, noclone)) void
foo (int *p)
{
  #pragma omp for reduction(+:p[:4])
  for (int i = 0; i < 64; i++)
    {
      p[0] += i;
      p[1] += i / 2;
      p[2] += 2 * i;
      p[3] += 3 * i;
    }
}

int
main ()
{
  int p[4] = { 0, 0, 0, 0 };
  #pragma omp parallel
  foo (p);
  if (p[0] != 63 * 64 / 2
      || p[1] != 31 * 32
      || p[2] != 63 * 64
      || p[3] != 3 * 63 * 64 / 2)
    __builtin_abort ();
  return 0;
}
