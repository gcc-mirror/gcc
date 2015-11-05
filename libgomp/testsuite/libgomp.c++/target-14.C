extern "C" void abort ();
int x;

__attribute__((noinline, noclone)) void
foo (int &a, int (&b)[10], short &c, long (&d)[5], int n)
{
  int err;
  int &t = x;
  int y[n + 1];
  int (&z)[n + 1] = y;
  for (int i = 0; i < n + 1; i++)
    z[i] = i + 27;
  #pragma omp target enter data map (to: z, c) map (alloc: b, t)
  #pragma omp target update to (b, t)
  #pragma omp target map (tofrom: a, d) map (from: b, c) map (alloc: t, z) map (from: err)
  {
    err = a++ != 7;
    for (int i = 0; i < 10; i++)
      {
	err |= b[i] != 10 - i;
	b[i] = i - 16;
	if (i >= 6) continue;
	err |= z[i] != i + 27;
	z[i] = 2 * i + 9;
	if (i == 5) continue;
	err |= d[i] != 12L + i;
	d[i] = i + 7;
      }
    err |= c != 25;
    c = 142;
    err |= t != 8;
    t = 19;
  }
  if (err) abort ();
  #pragma omp target update from (z, c)
  #pragma omp target exit data map (from: b, t) map (release: z, c)
  if (a != 8 || c != 142 || t != 19)
    abort ();
  a = 29;
  c = 149;
  t = 15;
  for (int i = 0; i < 10; i++)
    {
      if (b[i] != i - 16) abort ();
      b[i] = i ^ 1;
      if (i >= 6) continue;
      if (z[i] != 2 * i + 9) abort ();
      z[i]++;
      if (i == 5) continue;
      if (d[i] != i + 7) abort ();
      d[i] = 7 - i;
    }
  #pragma omp target defaultmap(tofrom: scalar)
  {
    err = a++ != 29;
    for (int i = 0; i < 10; i++)
      {
	err |= b[i] != i ^ 1;
	b[i] = i + 5;
	if (i >= 6) continue;
	err |= z[i] != 2 * i + 10;
	z[i] = 9 - 3 * i;
	if (i == 5) continue;
	err |= d[i] != 7L - i;
	d[i] = i;
      }
    err |= c != 149;
    c = -2;
    err |= t != 15;
    t = 155;
  }
  if (err || a != 30 || c != -2 || t != 155)
    abort ();
  for (int i = 0; i < 10; i++)
    {
      if (b[i] != i + 5) abort ();
      if (i >= 6) continue;
      if (z[i] != 9 - 3 * i) abort ();
      z[i]++;
      if (i == 5) continue;
      if (d[i] != i) abort ();
    }
  #pragma omp target data map (alloc: z)
  {
    #pragma omp target update to (z)
    #pragma omp target map(from: err)
    {
      err = 0;
      for (int i = 0; i < 6; i++)
	if (z[i] != 10 - 3 * i) err = 1;
	else z[i] = i;
    }
    if (err) abort ();
    #pragma omp target update from (z)
  }
  for (int i = 0; i < 6; i++)
    if (z[i] != i)
      abort ();
}

int
main ()
{
  int a = 7;
  int b[10] = { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
  short c = 25;
  long d[5] = { 12, 13, 14, 15, 16 };
  x = 8;
  foo (a, b, c, d, 5);
}
