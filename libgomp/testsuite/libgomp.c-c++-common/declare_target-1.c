/* PR c++/99509  */

#pragma omp declare target
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
