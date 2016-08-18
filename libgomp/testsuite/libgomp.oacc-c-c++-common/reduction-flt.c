
/* Single float has 23 bits of fraction. */
#define FRAC (1.0f / (1 << 20))
typedef float Type;

int close_enough (Type a, Type b)
{
  Type diff = a - b;
  if (diff < 0)
    diff = -diff;

  return diff / a < FRAC;
}

#define N 100

static int __attribute__ ((noinline))
vector (Type ary[N], Type sum, Type prod)
{
  Type tsum = 0, tprod = 1;

#pragma acc parallel vector_length(32) copyin(ary[0:N])
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
worker (Type ary[N], Type sum, Type prod)
{
  Type tsum = 0, tprod = 1;

#pragma acc parallel num_workers(32) copyin(ary[0:N])
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
gang (Type ary[N], Type sum, Type prod)
{
  Type tsum = 0, tprod = 1;

#pragma acc parallel num_gangs (32) copyin(ary[0:N])
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
  Type ary[N], sum = 0, prod = 1;

  for (int ix = 0; ix < N;  ix++)
    {
      float frac = ix * (1.0f / 1024) + 1.0f;
      
      ary[ix] = frac;
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
