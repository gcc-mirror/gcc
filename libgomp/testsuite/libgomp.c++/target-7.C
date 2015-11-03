extern "C" void abort ();

void
foo (int *x, int *&y, int (&z)[15])
{
  int a[10], b[15], err, i;
  for (i = 0; i < 10; i++)
    a[i] = 7 * i;
  for (i = 0; i < 15; i++)
    b[i] = 8 * i;
  #pragma omp target map(to:x[5:10], y[5:10], z[5:10], a[0:10], b[5:10]) map(from:err)
  {
    err = 0;
    for (i = 0; i < 10; i++)
      if (x[5 + i] != 20 + 4 * i
	  || y[5 + i] != 25 + 5 * i
	  || z[5 + i] != 30 + 6 * i
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
  int (*y2)[n] = &d;
  int (*&y)[n] = y2;
  int (&z)[n] = e;
  for (i = 0; i < n; i++)
    {
      (*x)[i] = 4 * i;
      (*y)[i] = 5 * i;
      z[i] = 6 * i;
      a[i] = 7 * i;
      b[i] = 8 * i;
    }
  #pragma omp target map(to:x[0][5:10], y[0][5:10], z[5:10], a[0:10], b[5:10]) map(from:err)
  {
    err = 0;
    for (i = 0; i < 10; i++)
      if ((*x)[5 + i] != 20 + 4 * i
	  || (*y)[5 + i] != 25 + 5 * i
	  || z[5 + i] != 30 + 6 * i
	  || a[i] != 7 * i
	  || b[5 + i] != 40 + 8 * i)
	err = 1;
  }
  if (err)
    abort ();
  for (i = 0; i < n; i++)
    {
      (*x)[i] = 9 * i;
      (*y)[i] = 10 * i;
      z[i] = 11 * i;
      a[i] = 12 * i;
      b[i] = 13 * i;
    }
  #pragma omp target map(to:x[0][v:v+5], y[0][v:v+5], z[v:v+5], a[v-5:v+5], b[v:v+5]) map(from:err)
  {
    err = 0;
    for (i = 0; i < 10; i++)
      if ((*x)[5 + i] != 45 + 9 * i
	  || (*y)[5 + i] != 50 + 10 * i
	  || z[5 + i] != 55 + 11 * i
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
  int x[15], y2[15], z[15], *y = y2, i;
  for (i = 0; i < 15; i++)
    {
      x[i] = 4 * i;
      y[i] = 5 * i;
      z[i] = 6 * i;
    }
  foo (x, y, z);
  bar (15, 5);
}
