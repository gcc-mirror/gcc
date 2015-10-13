extern void abort (void);

void
foo (int n)
{
  int a[n], i, err;
  for (i = 0; i < n; i++)
    a[i] = 7 * i;
  #pragma omp target firstprivate (a) map(from:err) private (i)
  {
    err = 0;
    for (i = 0; i < n; i++)
      if (a[i] != 7 * i)
	err = 1;
  }
  if (err)
    abort ();
}

void
bar (int n)
{
  int a[n], i, err;
  #pragma omp target private (a) map(from:err)
  {
    #pragma omp parallel for
    for (i = 0; i < n; i++)
      a[i] = 7 * i;
    err = 0;
    #pragma omp parallel for reduction(|:err)
    for (i = 0; i < n; i++)
      if (a[i] != 7 * i)
	err |= 1;
  }
  if (err)
    abort ();
}

int
main ()
{
  foo (7);
  bar (7);
  return 0;
}
