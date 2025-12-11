/* PR c++/99509  */
#pragma omp declare target // { dg-warning "use of 'omp declare target' as a synonym for 'omp begin declare target' has been deprecated since OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" }
int data[] = {5};
#pragma omp end declare target

static inline int
foo (int idx)
{
  return data[idx];
}

int
main ()
{
  int i = -1;
  #pragma omp target map(from:i)
    i = foo(0);
  if (i != 5)
    __builtin_abort ();
  return 0;
}
