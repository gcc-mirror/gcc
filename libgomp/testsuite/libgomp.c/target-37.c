extern void abort (void);
struct S { int e, f; };

void
foo (int n)
{
  int a[4] = { 0, 1, 2, 3 }, b[n], c = 4;
  struct S d = { 5, 6 };
  int *p = a + 1, i, err;
  for (i = 0; i < n; i++)
    b[i] = 9 + i;
  #pragma omp target data use_device_ptr(p) map(from:err) map(to:a)
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
  #pragma omp target data map(to:a) use_device_addr(a) map(from:err)
  #pragma omp target is_device_ptr(a) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (a[i] != 23 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data use_device_addr(b) map(from:err) map(to:b)
  #pragma omp target is_device_ptr(b) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (b[i] != 9 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:c) use_device_addr(c) map(from:err)
  {
    int *q = &c;
    #pragma omp target is_device_ptr(q) map(from:err)
    {
      err = *q != 4;
    }
  }
  if (err)
    abort ();
  #pragma omp target data use_device_addr(d) map(to:d) map(from:err)
  {
    struct S *r = &d;
    #pragma omp target is_device_ptr(r) map(from:err)
    {
      err = r->e != 5 || r->f != 6;
    }
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
