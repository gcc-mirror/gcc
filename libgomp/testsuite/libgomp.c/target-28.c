extern void abort (void);

int g;
#pragma omp declare target (g)

#pragma omp declare target
int
foo (void)
{
  static int s;
  return ++s + g;
}
#pragma omp end declare target

int
bar (void)
{
  static int s;
  #pragma omp declare target to (s)
  return ++s;
}
#pragma omp declare target (bar)

int
main ()
{
  int r;
  #pragma omp target map(from:r)
  {
    r = (foo () == 1) + (bar () == 1);
    r += (foo () == 2) + (bar () == 2);
  }
  if (r != 4)
    abort ();
  return 0;
}
