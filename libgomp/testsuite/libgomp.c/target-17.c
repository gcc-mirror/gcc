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
}

int
main ()
{
  foo (9);
  return 0;
}
