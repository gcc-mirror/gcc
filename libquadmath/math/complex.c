#include "quadmath-imp.h"


#define REALPART(z) (__real__(z)) 
#define IMAGPART(z) (__imag__(z)) 
#define COMPLEX_ASSIGN(z_, r_, i_) {__real__(z_) = (r_); __imag__(z_) = (i_);} 


// Horrible... GCC doesn't know how to multiply or divide these
// __complex128 things. We have to do it on our own.
// Protect it around macros so, some day, we can switch it on

#if 0

# define C128_MULT(x,y) ((x)*(y))
# define C128_DIV(x,y) ((x)/(y))

#else

#define C128_MULT(x,y) mult_c128(x,y)
#define C128_DIV(x,y) div_c128(x,y)

static inline __complex128 mult_c128 (__complex128 x, __complex128 y)
{
  __float128 r1 = REALPART(x), i1 = IMAGPART(x);
  __float128 r2 = REALPART(y), i2 = IMAGPART(y);
  __complex128 res;
  COMPLEX_ASSIGN(res, r1*r2 - i1*i2, i2*r1 + i1*r2);
  return res;
}


// Careful: the algorithm for the division sucks. A lot.
static inline __complex128 div_c128 (__complex128 x, __complex128 y)
{
  __float128 n = hypotq (REALPART (y), IMAGPART (y));
  __float128 r1 = REALPART(x), i1 = IMAGPART(x);
  __float128 r2 = REALPART(y), i2 = IMAGPART(y);
  __complex128 res;
  COMPLEX_ASSIGN(res, r1*r2 + i1*i2, i1*r2 - i2*r1);
  return res / n;
}

#endif



__float128
cabsq (__complex128 z)
{
  return hypotq (REALPART (z), IMAGPART (z));
}


__complex128
cexpq (__complex128 z)
{
  __float128 a, b;
  __complex128 v;

  a = REALPART (z);
  b = IMAGPART (z);
  COMPLEX_ASSIGN (v, cosq (b), sinq (b));
  return expq (a) * v;
}


__complex128
cexpiq (__float128 x)
{
  __complex128 v;
  COMPLEX_ASSIGN (v, cosq (x), sinq (x));
  return v;
}


__float128
cargq (__complex128 z)
{
  return atan2q (IMAGPART (z), REALPART (z));
}


__complex128
clogq (__complex128 z)
{
  __complex128 v;
  COMPLEX_ASSIGN (v, logq (cabsq (z)), cargq (z));
  return v;
}


__complex128
clog10q (__complex128 z)
{
  __complex128 v;
  COMPLEX_ASSIGN (v, log10q (cabsq (z)), cargq (z));
  return v;
}


__complex128
cpowq (__complex128 base, __complex128 power)
{
  return cexpq (C128_MULT(power, clogq (base)));
}


__complex128
csinq (__complex128 a)
{
  __float128 r = REALPART (a), i = IMAGPART (a);
  __complex128 v;
  COMPLEX_ASSIGN (v, sinq (r) * coshq (i), cosq (r) * sinhq (i));
  return v;
}


__complex128
csinhq (__complex128 a)
{
  __float128 r = REALPART (a), i = IMAGPART (a);
  __complex128 v;
  COMPLEX_ASSIGN (v, sinhq (r) * cosq (i), coshq (r) * sinq (i));
  return v;
}


__complex128
ccosq (__complex128 a)
{
  __float128 r = REALPART (a), i = IMAGPART (a);
  __complex128 v;
  COMPLEX_ASSIGN (v, cosq (r) * coshq (i), - (sinq (r) * sinhq (i)));
  return v;
}


__complex128
ccoshq (__complex128 a)
{
  __float128 r = REALPART (a), i = IMAGPART (a);
  __complex128 v;
  COMPLEX_ASSIGN (v, coshq (r) * cosq (i),  sinhq (r) * sinq (i));
  return v;
}


__complex128
ctanq (__complex128 a)
{
  __float128 rt = tanq (REALPART (a)), it = tanhq (IMAGPART (a));
  __complex128 n, d;
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));
  return C128_DIV(n,d);
}


__complex128
ctanhq (__complex128 a)
{
  __float128 rt = tanhq (REALPART (a)), it = tanq (IMAGPART (a));
  __complex128 n, d;
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, rt * it);
  return C128_DIV(n,d);
}


/* Square root algorithm from glibc.  */
__complex128
csqrtq (__complex128 z)
{
  __float128 re = REALPART(z), im = IMAGPART(z);
  __complex128 v;

  if (im == 0)
  {
    if (re < 0)
    {
      COMPLEX_ASSIGN (v, 0, copysignq (sqrtq (-re), im));
    }
    else
    {
      COMPLEX_ASSIGN (v, fabsq (sqrtq (re)), copysignq (0, im));
    }
  }
  else if (re == 0)
  {
    __float128 r = sqrtq (0.5 * fabsq (im));
    COMPLEX_ASSIGN (v, r, copysignq (r, im));
  }
  else
  {
    __float128 d = hypotq (re, im);
    __float128 r, s;

    /* Use the identity   2  Re res  Im res = Im x
	to avoid cancellation error in  d +/- Re x.  */
    if (re > 0)
      r = sqrtq (0.5 * d + 0.5 * re), s = (0.5 * im) / r;
    else
      s = sqrtq (0.5 * d - 0.5 * re), r = fabsq ((0.5 * im) / s);

    COMPLEX_ASSIGN (v, r, copysignq (s, im));
  }
  return v;
}

