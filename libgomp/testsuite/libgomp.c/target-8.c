/* { dg-do run } */

void
foo (int *p)
{
  int i;
  #pragma omp parallel
  #pragma omp single
  #pragma omp target teams distribute parallel for map(p[0:24])
  for (i = 0; i < 24; i++)
    p[i] = p[i] + 1;
}

int
main ()
{
  int p[24], i;
  for (i = 0; i < 24; i++)
    p[i] = i;
  foo (p);
  for (i = 0; i < 24; i++)
    if (p[i] != i + 1)
      __builtin_abort ();
  return 0;
}
