
#include <complex.h>

/* Double float has 53 bits of fraction. */
#define FRAC (1.0 / (1LL << 48))

int close_enough (double _Complex a, double _Complex b)
{
  double _Complex diff = a - b;
  double mag2_a = __real__(a) * __real__ (a) + __imag__ (a) * __imag__ (a);
  double mag2_diff = (__real__(diff) * __real__ (diff)
		     + __imag__ (diff) * __imag__ (diff));

  return mag2_diff / mag2_a < (FRAC * FRAC);
}

int main (void)
{
#define N 100
  double _Complex ary[N], sum, prod, tsum, tprod;
  int ix;

  sum = tsum = 0;
  prod = tprod = 1;
  
  for (ix = 0; ix < N;  ix++)
    {
      double frac = ix * (1.0 / 1024) + 1.0;
      
      ary[ix] = frac + frac * 2.0i - 1.0i;
      sum += ary[ix];
      prod *= ary[ix];
    }

#pragma acc parallel vector_length(32) copyin(ary) copy (tsum, tprod)
  {
#pragma acc loop vector reduction(+:tsum) reduction (*:tprod)
    for (ix = 0; ix < N; ix++)
      {
	tsum += ary[ix];
	tprod *= ary[ix];
      }
  }

  if (!close_enough (sum, tsum))
    return 1;

  if (!close_enough (prod, tprod))
    return 1;

  return 0;
}
