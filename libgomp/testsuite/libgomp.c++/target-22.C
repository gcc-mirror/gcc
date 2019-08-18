extern "C" void abort (void);
struct S { int e, f; };

void
foo (int *&p, int (&s)[5], int &t, S &u, int n)
{
  int a[4] = { 7, 8, 9, 10 }, b[n], c[3] = { 20, 21, 22 };
  int *r = a + 1, *q = p - 1, i, err;
  int v = 27;
  S w = { 28, 29 };
  for (i = 0; i < n; i++)
    b[i] = 9 + i;
  #pragma omp target data map(to:a) use_device_ptr(r) map(from:err)
  #pragma omp target is_device_ptr(r) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (r[i - 1] != 7 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data use_device_ptr(p) map(from:err) map(to:q[:4])
  #pragma omp target is_device_ptr(p) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 4; i++)
      if (p[i - 1] != i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:b) use_device_addr(b) map(from:err)
  #pragma omp target is_device_ptr(b) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (b[i] != 9 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data use_device_addr(c) map(to:c) map(from:err)
  #pragma omp target is_device_ptr(c) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 3; i++)
      if (c[i] != 20 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data map(to:s[:5]) use_device_addr(s) map(from:err)
  #pragma omp target is_device_ptr(s) private(i) map(from:err)
  {
    err = 0;
    for (i = 0; i < 5; i++)
      if (s[i] != 17 + i)
	err = 1;
  }
  if (err)
    abort ();
  #pragma omp target data use_device_addr (v) map(to: v) map(to:u) use_device_addr (u) map(from:err)
  {
    int *z = &v;
    S *x = &u;
    #pragma omp target is_device_ptr (z, x) map(from:err)
    {
      err = 0;
      if (*z != 27 || x->e != 25 || x->f != 26)
	err = 1;
    }
  }
  if (err)
    abort ();
  #pragma omp target data map(to: t) use_device_addr (t, w) map (to: w) map(from:err)
  {
    int *z = &t;
    S *x = &w;
    #pragma omp target is_device_ptr (z) is_device_ptr (x) map(from:err)
    {
      err = 0;
      if (*z != 24 || x->e != 28 || x->f != 29)
	err = 1;
    }
  }
  if (err)
    abort ();
}

int
main ()
{
  int a[4] = { 0, 1, 2, 3 }, b[5] = { 17, 18, 19, 20, 21 };
  int *p = a + 1;
  int t = 24;
  S u = { 25, 26 };
  foo (p, b, t, u, 9);
}
