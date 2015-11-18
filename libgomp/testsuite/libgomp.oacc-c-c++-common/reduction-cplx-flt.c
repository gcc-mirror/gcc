
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

#define N 100

static int __attribute__ ((noinline))
vector (float _Complex ary[N], float _Complex sum, float _Complex prod)
{
  float _Complex tsum = 0, tprod = 1;

#pragma acc parallel vector_length(32) copyin(ary[0:N]) copy (tsum, tprod)
  {
#pragma acc loop vector reduction(+:tsum) reduction (*:tprod)
    for (int ix = 0; ix < N; ix++)
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

static int __attribute__ ((noinline))
worker (float _Complex ary[N], float _Complex sum, float _Complex prod)
{
  float _Complex tsum = 0, tprod = 1;

#pragma acc parallel num_workers(32) copyin(ary[0:N]) copy (tsum, tprod)
  {
#pragma acc loop worker reduction(+:tsum) reduction (*:tprod)
    for (int ix = 0; ix < N; ix++)
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

static int __attribute__ ((noinline))
gang (float _Complex ary[N], float _Complex sum, float _Complex prod)
{
  float _Complex tsum = 0, tprod = 1;

#pragma acc parallel num_gangs (32) copyin(ary[0:N]) copy (tsum, tprod)
  {
#pragma acc loop gang reduction(+:tsum) reduction (*:tprod)
    for (int ix = 0; ix < N; ix++)
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

int main (void)
{
  float _Complex ary[N], sum = 0, prod = 1;

  for (int ix = 0; ix < N;  ix++)
    {
      float frac = ix * (1.0f / 1024) + 1.0f;
      
      ary[ix] = frac + frac * 2.0i - 1.0i;
      sum += ary[ix];
      prod *= ary[ix];
    }

  if (vector (ary, sum, prod))
    return 1;
  
  if (worker (ary, sum, prod))
    return 1;

  if (gang (ary, sum, prod))
    return 1;

  return 0;
}
