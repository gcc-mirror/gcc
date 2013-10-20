// { dg-options "-O2 -fopenmp" }
// { dg-additional-sources "target-2-aux.cc" }

extern "C" void abort (void);

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

double b[1024];
double (&br) [1024] = b;
double cbuf[1024];
double *c = cbuf;
double *&cr = c;
extern double (&fr) [1024];
extern double *&gr;

double
fn2 (int x, double (&dr) [1024], double *&er)
{
  double s = 0;
  double h[1024];
  double (&hr) [1024] = h;
  double ibuf[1024];
  double *i = ibuf;
  double *&ir = i;
  int j;
  fn1 (hr + 2 * x, ir + 2 * x, x);
  #pragma omp target map(to: br[:x], cr[0:x], dr[x:x], er[x:x]) \
		     map(to: fr[0:x], gr[0:x], hr[2 * x:x], ir[2 * x:x])
    #pragma omp parallel for reduction(+:s)
      for (j = 0; j < x; j++)
	s += br[j] * cr[j] + dr[x + j] + er[x + j]
	     + fr[j] + gr[j] + hr[2 * x + j] + ir[2 * x + j];
  return s;
}

int
main ()
{
  double d[1024];
  double ebuf[1024];
  double *e = ebuf;
  fn1 (br, cr, 128);
  fn1 (d + 128, e + 128, 128);
  fn1 (fr, gr, 128);
  double h = fn2 (128, d, e);
  if (h != 20416.0)
    abort ();
  return 0;
}
