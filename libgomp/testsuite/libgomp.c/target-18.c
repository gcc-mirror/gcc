extern void abort (void);

void
foo (int n)
{
  int a[4] = { 0, 1, 2, 3 }, b[n];
  int *p = a + 1, i, err;
  for (i = 0; i < n; i++)
    b[i] = 9 + i;
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_ptr(p) map(from:err)
  #pragma omp target is_device_ptr(p) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (p[i - 1] != i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < 4; i++)
    a[i] = 23 + i;
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_ptr(a) map(from:err)
  #pragma omp target is_device_ptr(a) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (a[i] != 23 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:b)
  #pragma omp target data use_device_ptr(b) map(from:err)
  #pragma omp target is_device_ptr(b) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (b[i] != 9 + i)
	err = 1;
  }
  if (err)
    abort ();
}

int
main ()
{
  foo (9);
  return 0;
}
