extern void abort (void);

void
foo (int n)
{
  int a[n], i, err;
  for (i = 0; i < n; i++)
    a[i] = 5 * i;
  #pragma omp target map(to:a) map(from:err) private(i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (a[i] != 5 * i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    a[i] += i;
  #pragma omp target map(from:err) private(i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (a[i] != 6 * i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    a[i] += i;
  #pragma omp target firstprivate (a) map(from:err) private(i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (a[i] != 7 * i)
	err = 1;
  }
  if (err)
    abort ();
  int on = n;
  #pragma omp target firstprivate (n) map(tofrom: n)
  {
    n++;
  }
  if (on != n)
    abort ();
  #pragma omp target map(tofrom: n) private (n)
  {
    n = 25;
  }
  if (on != n)
    abort ();
  for (i = 0; i < n; i++)
    a[i] += i;
  #pragma omp target map(to:a) firstprivate (a) map(from:err) private(i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (a[i] != 8 * i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    a[i] += i;
  #pragma omp target firstprivate (a) map(to:a) map(from:err) private(i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (a[i] != 9 * i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    a[i] += i;
  #pragma omp target map(tofrom:a) map(from:err) private(a, i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      a[i] = 7;
    #pragma omp parallel for reduction(|:err)
    for (i = 0; i < n; i++)
      if (a[i] != 7)
	err |= 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    if (a[i] != 10 * i)
      abort ();
}

int
main ()
{
  foo (9);
  return 0;
}
