// { dg-do run }
// { dg-require-effective-target lto }
// { dg-options "-flto -g -fopenmp" }

int e = 5;

int
main ()
{
  int a[e];
  a[0] = 6;
#pragma omp parallel
  if (a[0] != 6)
    __builtin_abort ();
  return 0;
}
