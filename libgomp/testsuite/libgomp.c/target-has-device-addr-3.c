/* Testing 'has_device_addr' clause with variable sized array. */

int
foo (int size)
{
  int x[size];

  #pragma omp target data map(x[:size]) use_device_addr(x)
    #pragma omp target has_device_addr(x)
      for (int i = 0; i < size; i++)
	x[i] = i;
  for (int i = 0; i < size; i++)
    if (x[i] != i)
      __builtin_abort ();

  #pragma omp target data map(x) use_device_addr(x)
    #pragma omp target has_device_addr(x[2:3])
      for (int i = 0; i < size; i++)
	x[i] = i;
  for (int i = 0; i < size; i++)
    if (x[i] != i)
      __builtin_abort ();

  return 0;
}

int
main ()
{
  foo (40);

  return 0;
}
