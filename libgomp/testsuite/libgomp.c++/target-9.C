extern "C" void abort (void);

void
foo (int *&p, int (&s)[5], int n)
{
  int a[4] = { 7, 8, 9, 10 }, b[n], c[3] = { 20, 21, 22 };
  int *r = a + 1, *q = p - 1, i, err;
  for (i = 0; i < n; i++)
    b[i] = 9 + i;
  #pragma omp target data map(to:a)
  #pragma omp target data use_device_ptr(r) map(from:err)
  #pragma omp target is_device_ptr(r) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (r[i - 1] != 7 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:q[:4])
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
  #pragma omp target data map(to:b)
  #pragma omp target data use_device_ptr(b) map(from:err)
  #pragma omp target is_device_ptr(b) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (b[i] != 9 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:c)
  #pragma omp target data use_device_ptr(c) map(from:err)
  #pragma omp target is_device_ptr(c) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 3; i++)
      if (c[i] != 20 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:s[:5])
  #pragma omp target data use_device_ptr(s) map(from:err)
  #pragma omp target is_device_ptr(s) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 5; i++)
      if (s[i] != 17 + i)
	err = 1;
  }
  if (err)
    abort ();
}

int
main ()
{
  int a[4] = { 0, 1, 2, 3 }, b[5] = { 17, 18, 19, 20, 21 };
  int *p = a + 1;
  foo (p, b, 9);
}
