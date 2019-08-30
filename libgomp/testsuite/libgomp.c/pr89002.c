/* PR middle-end/89002 */

extern void abort (void);

int
foo (int x)
{
  int a;
  int *p = &a;

#pragma omp taskloop lastprivate (a)
  for (a = 0; a < x; ++a)
    ;
  return *p;
}

int
bar (int x)
{
  int a;
  int *p = &a;

#pragma omp parallel
#pragma omp single
#pragma omp taskloop lastprivate (a)
  for (a = 0; a < x; ++a)
    ;
  return *p;
}

int
main ()
{
#pragma omp parallel
#pragma omp single
  {
    if (foo (4) != 4)
      abort ();
  }
  if (bar (6) != 6)
    abort ();
  return 0;
}
