/* Testing the 'has_device_addr' clause on the target construct with
   enclosing 'target data' construct. */

#define N 40

int
main ()
{
  int x = 24;

  #pragma omp target data map(x) use_device_addr(x)
    #pragma omp target has_device_addr(x)
      x = 42;
  if (x != 42)
    __builtin_abort ();

  int y[N];

  for (int i = 0; i < N; i++)
    y[i] = 42;
  #pragma omp target data map(y) use_device_addr(y)
    #pragma omp target has_device_addr(y)
      for (int i = 0; i < N; i++)
	y[i] = i;
  for (int i = 0; i < N; i++)
    if (y[i] != i)
      __builtin_abort ();

  #pragma omp target data map(y[:N]) use_device_addr(y)
    #pragma omp target has_device_addr(y[:N])
      for (int i = 0; i < N; i++)
	y[i] = i + 2;
  for (int i = 0; i < N; i++)
    if (y[i] != i + 2)
      __builtin_abort ();

  #pragma omp target data map(y[:N]) use_device_addr(y)
    #pragma omp target has_device_addr(y[24])
	y[24] = 42;
  if (y[24] != 42)
    __builtin_abort ();

  #pragma omp target data map(y[:N]) use_device_addr(y)
    #pragma omp target has_device_addr(y[24:])
      for (int i = 24; i < N; i++)
	y[i] = i + 3;
  for (int i = 24; i < N; i++)
    if (y[i] != i + 3)
      __builtin_abort ();

  #pragma omp target data map(y[:N]) use_device_addr(y)
    #pragma omp target has_device_addr(y[12:24])
      for (int i = 12; i < 24; i++)
	y[i] = i + 4;
  for (int i = 12; i < 24; i++)
    if (y[i] != i + 4)
      __builtin_abort ();

  int u[0];
  #pragma omp target data map(u) use_device_addr(u)
    #pragma omp target has_device_addr(u)
  ;

  struct S { int m; } s;
  s.m = 42;
  #pragma omp target data map (s) use_device_addr (s)
    #pragma omp target has_device_addr (s)
      ++s.m;
  if (s.m != 43)
    __builtin_abort ();

  return 0;
}
