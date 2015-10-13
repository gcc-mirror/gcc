extern void abort (void);

void
foo (int *x)
{
  int a[10], b[15], err, i;
  for (i = 0; i < 10; i++)
    a[i] = 7 * i;
  for (i = 0; i < 15; i++)
    b[i] = 8 * i;
  #pragma omp target map(to:x[5:10], a[0:10], b[5:10]) map(from:err)
  {
    err = 0;
    for (i = 0; i < 10; i++)
      if (x[5 + i] != 20 + 4 * i
	  || a[i] != 7 * i
	  || b[5 + i] != 40 + 8 * i)
	err = 1;
  }
  if (err)
    abort ();
}

void
bar (int n, int v)
{
  int a[n], b[n], c[n], d[n], e[n], err, i;
  int (*x)[n] = &c;
  for (i = 0; i < n; i++)
    {
      (*x)[i] = 4 * i;
      a[i] = 7 * i;
      b[i] = 8 * i;
    }
  #pragma omp target map(to:x[0][5:10], a[0:10], b[5:10]) map(from:err)
  {
    err = 0;
    for (i = 0; i < 10; i++)
      if ((*x)[5 + i] != 20 + 4 * i
	  || a[i] != 7 * i
	  || b[5 + i] != 40 + 8 * i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    {
      (*x)[i] = 9 * i;
      a[i] = 12 * i;
      b[i] = 13 * i;
    }
  #pragma omp target map(to:x[0][v:v+5], a[v-5:v+5], b[v:v+5]) map(from:err)
  {
    err = 0;
    for (i = 0; i < 10; i++)
      if ((*x)[5 + i] != 45 + 9 * i
	  || a[i] != 12 * i
	  || b[5 + i] != 65 + 13 * i)
	err = 1;
  }
  if (err)
    abort ();
}

int
main ()
{
  int x[15], i;
  for (i = 0; i < 15; i++)
    x[i] = 4 * i;
  foo (x);
  bar (15, 5);
  return 0;
}
