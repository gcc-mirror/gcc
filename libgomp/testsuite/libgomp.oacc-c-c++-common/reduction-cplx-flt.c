
#include <complex.h>

/* Single float has 23 bits of fraction. */
#define FRAC (1.0f / (1 << 20))

int close_enough (float _Complex a, float _Complex b)
{
  float _Complex diff = a - b;
  float mag2_a = __real__(a) * __real__ (a) + __imag__ (a) * __imag__ (a);
  float mag2_diff = (__real__(diff) * __real__ (diff)
		     + __imag__ (diff) * __imag__ (diff));

  return mag2_diff / mag2_a < (FRAC * FRAC);
}

int main (void)
{
#define N 100
  float _Complex ary[N], sum, prod, tsum, tprod;
  int ix;

  sum = tsum = 0;
  prod = tprod = 1;
  
  for (ix = 0; ix < N;  ix++)
    {
      float frac = ix * (1.0f / 1024) + 1.0f;
      
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
