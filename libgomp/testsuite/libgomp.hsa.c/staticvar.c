extern void abort (void);

#pragma omp declare target
int
foo (void)
{
  static int s;
  return ++s;
}
#pragma omp end declare target

int
main ()
{
  int r;
  #pragma omp target map(from:r)
  {
    r = foo ();
  }
  if (r != 1)
    abort ();
  return 0;
}
