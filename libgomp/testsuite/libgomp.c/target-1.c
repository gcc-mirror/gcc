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

#pragma omp declare target
int tgtv = 6;
int
tgt (void)
{
  #pragma omp atomic update
    tgtv++;
  return 0;
}
#pragma omp end declare target

double
fn2 (int x, int y, int z)
{
  double b[1024], c[1024], s = 0;
  int i, j;
  fn1 (b, c, x);
  #pragma omp target data map(to: b)
  {
    #pragma omp target map(tofrom: c, s)
      #pragma omp teams num_teams(y) thread_limit(z) reduction(+:s) firstprivate(x)
	#pragma omp distribute dist_schedule(static, 4) collapse(1)
	  for (j=0; j < x; j += y)
	    #pragma omp parallel for reduction(+:s)
	      for (i = j; i < j + y; i++)
		tgt (), s += b[i] * c[i];
    #pragma omp target update from(b, tgtv)
  }
  return s;
}

double
fn3 (int x)
{
  double b[1024], c[1024], s = 0;
  int i;
  fn1 (b, c, x);
  #pragma omp target map(to: b, c) map(tofrom:s)
    #pragma omp parallel for reduction(+:s)
      for (i = 0; i < x; i++)
	tgt (), s += b[i] * c[i];
  return s;
}

double
fn4 (int x, double *p)
{
  double b[1024], c[1024], d[1024], s = 0;
  int i;
  fn1 (b, c, x);
  fn1 (d + x, p + x, x);
  #pragma omp target map(to: b, c[0:x], d[x:x]) map(to:p[x:64 + (x & 31)]) \
		     map(tofrom: s)
    #pragma omp parallel for reduction(+:s)
      for (i = 0; i < x; i++)
	s += b[i] * c[i] + d[x + i] + p[x + i];
  return s;
}

int
main ()
{
  double a = fn2 (128, 4, 6);
  int b = tgtv;
  double c = fn3 (61);
  #pragma omp target update from(tgtv)
  int d = tgtv;
  double e[1024];
  double f = fn4 (64, e);
  if (a != 13888.0 || b != 6 + 128 || c != 4062.0 || d != 6 + 128 + 61
      || f != 8032.0)
    abort ();
  return 0;
}
