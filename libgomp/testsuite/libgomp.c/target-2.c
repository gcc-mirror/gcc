extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

void
fn1 (double *x, double *y, int z)
{
  int i;
  for (i = 0; i < z; i++)
    {
      x[i] = i & 31;
      y[i] = (i & 63) - 30;
    }
}

double
fn2 (int x)
{
  double s = 0;
  double b[3 * x], c[3 * x], d[3 * x], e[3 * x];
  int i;
  fn1 (b, c, x);
  fn1 (e, d + x, x);
  #pragma omp target map(to: b, c[:x], d[x:x], e) map(tofrom: s)
    #pragma omp parallel for reduction(+:s)
      for (i = 0; i < x; i++)
	s += b[i] * c[i] + d[x + i] + sizeof (b) - sizeof (c);
  return s;
}

double
fn3 (int x)
{
  double s = 0;
  double b[3 * x], c[3 * x], d[3 * x], e[3 * x];
  int i;
  fn1 (b, c, x);
  fn1 (e, d, x);
  #pragma omp target map(tofrom: s)
    #pragma omp parallel for reduction(+:s)
      for (i = 0; i < x; i++)
	s += b[i] * c[i] + d[i];
  return s;
}

double
fn4 (int x)
{
  double s = 0;
  double b[3 * x], c[3 * x], d[3 * x], e[3 * x];
  int i;
  fn1 (b, c, x);
  fn1 (e, d + x, x);
  #pragma omp target data map(from: b, c[:x], d[x:x], e)
    {
      #pragma omp target update to(b, c[:x], d[x:x], e)
      #pragma omp target map(c[:x], d[x:x], s)
	#pragma omp parallel for reduction(+:s)
	  for (i = 0; i < x; i++)
	    {
	      s += b[i] * c[i] + d[x + i] + sizeof (b) - sizeof (c);
	      b[i] = i + 0.5;
	      c[i] = 0.5 - i;
	      d[x + i] = 0.5 * i;
	    }
    }
  for (i = 0; i < x; i++)
    if (b[i] != i + 0.5 || c[i] != 0.5 - i || d[x + i] != 0.5 * i)
      abort ();
  return s;
}

int
main ()
{
  double a = fn2 (128);
  if (a != 14080.0)
    abort ();
  double b = fn3 (128);
  if (a != b)
    abort ();
  double c = fn4 (256);
  if (c != 28160.0)
    abort ();
  return 0;
}
