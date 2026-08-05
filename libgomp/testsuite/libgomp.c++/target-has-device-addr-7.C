/* Testing 'has_device_addr' clause on the target construct with template.  */

template <typename T>
void
foo (T x)
{
  x = 24;
  #pragma omp target data map(x) use_device_addr(x)
    #pragma omp target has_device_addr(x)
      x = 42;

  if (x != 42)
    __builtin_abort ();
}

template <typename T>
void
bar (T (&x)[])
{
  x[0] = 24;
  #pragma omp target data map(x[:2]) use_device_addr(x)
    #pragma omp target has_device_addr(x[:2])
      x[0] = 42;

  if (x[0] != 42)
    __builtin_abort ();
}

int
main ()
{
  int a[] = { 24, 42};
  foo <int> (42);
  bar <int> (a);
  return 0;
}
