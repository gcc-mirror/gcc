/* Implementation of various C99 functions 
   Copyright (C) 2004, 2009 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"

#define C99_PROTOS_H WE_DONT_WANT_PROTOS_NOW
#include "libgfortran.h"

/* IRIX's <math.h> declares a non-C99 compliant implementation of cabs,
   which takes two floating point arguments instead of a single complex.
   If <complex.h> is missing this prevents building of c99_functions.c.
   To work around this we redirect cabs{,f,l} calls to __gfc_cabs{,f,l}.  */

#if defined(__sgi__) && !defined(HAVE_COMPLEX_H)
#undef HAVE_CABS
#undef HAVE_CABSF
#undef HAVE_CABSL
#define cabs __gfc_cabs
#define cabsf __gfc_cabsf
#define cabsl __gfc_cabsl
#endif
        
/* Tru64's <math.h> declares a non-C99 compliant implementation of cabs,
   which takes two floating point arguments instead of a single complex.
   To work around this we redirect cabs{,f,l} calls to __gfc_cabs{,f,l}.  */

#ifdef __osf__
#undef HAVE_CABS
#undef HAVE_CABSF
#undef HAVE_CABSL
#define cabs __gfc_cabs
#define cabsf __gfc_cabsf
#define cabsl __gfc_cabsl
#endif

/* Prototypes to silence -Wstrict-prototypes -Wmissing-prototypes.  */

float cabsf(float complex);
double cabs(double complex);
long double cabsl(long double complex);

float cargf(float complex);
double carg(double complex);
long double cargl(long double complex);

float complex clog10f(float complex);
double complex clog10(double complex);
long double complex clog10l(long double complex);


/* Wrappers for systems without the various C99 single precision Bessel
   functions.  */

#if defined(HAVE_J0) && ! defined(HAVE_J0F)
#define HAVE_J0F 1
extern float j0f (float);

float
j0f (float x)
{
  return (float) j0 ((double) x);
}
#endif

#if defined(HAVE_J1) && !defined(HAVE_J1F)
#define HAVE_J1F 1
extern float j1f (float);

float j1f (float x)
{
  return (float) j1 ((double) x);
}
#endif

#if defined(HAVE_JN) && !defined(HAVE_JNF)
#define HAVE_JNF 1
extern float jnf (int, float);

float
jnf (int n, float x)
{
  return (float) jn (n, (double) x);
}
#endif

#if defined(HAVE_Y0) && !defined(HAVE_Y0F)
#define HAVE_Y0F 1
extern float y0f (float);

float
y0f (float x)
{
  return (float) y0 ((double) x);
}
#endif

#if defined(HAVE_Y1) && !defined(HAVE_Y1F)
#define HAVE_Y1F 1
extern float y1f (float);

float
y1f (float x)
{
  return (float) y1 ((double) x);
}
#endif

#if defined(HAVE_YN) && !defined(HAVE_YNF)
#define HAVE_YNF 1
extern float ynf (int, float);

float
ynf (int n, float x)
{
  return (float) yn (n, (double) x);
}
#endif


/* Wrappers for systems without the C99 erff() and erfcf() functions.  */

#if defined(HAVE_ERF) && !defined(HAVE_ERFF)
#define HAVE_ERFF 1
extern float erff (float);

float
erff (float x)
{
  return (float) erf ((double) x);
}
#endif

#if defined(HAVE_ERFC) && !defined(HAVE_ERFCF)
#define HAVE_ERFCF 1
extern float erfcf (float);

float
erfcf (float x)
{
  return (float) erfc ((double) x);
}
#endif


#ifndef HAVE_ACOSF
#define HAVE_ACOSF 1
float
acosf(float x)
{
  return (float) acos(x);
}
#endif

#if HAVE_ACOSH && !HAVE_ACOSHF
float
acoshf (float x)
{
  return (float) acosh ((double) x);
}
#endif

#ifndef HAVE_ASINF
#define HAVE_ASINF 1
float
asinf(float x)
{
  return (float) asin(x);
}
#endif

#if HAVE_ASINH && !HAVE_ASINHF
float
asinhf (float x)
{
  return (float) asinh ((double) x);
}
#endif

#ifndef HAVE_ATAN2F
#define HAVE_ATAN2F 1
float
atan2f(float y, float x)
{
  return (float) atan2(y, x);
}
#endif

#ifndef HAVE_ATANF
#define HAVE_ATANF 1
float
atanf(float x)
{
  return (float) atan(x);
}
#endif

#if HAVE_ATANH && !HAVE_ATANHF
float
atanhf (float x)
{
  return (float) atanh ((double) x);
}
#endif

#ifndef HAVE_CEILF
#define HAVE_CEILF 1
float
ceilf(float x)
{
  return (float) ceil(x);
}
#endif

#ifndef HAVE_COPYSIGNF
#define HAVE_COPYSIGNF 1
float
copysignf(float x, float y)
{
  return (float) copysign(x, y);
}
#endif

#ifndef HAVE_COSF
#define HAVE_COSF 1
float
cosf(float x)
{
  return (float) cos(x);
}
#endif

#ifndef HAVE_COSHF
#define HAVE_COSHF 1
float
coshf(float x)
{
  return (float) cosh(x);
}
#endif

#ifndef HAVE_EXPF
#define HAVE_EXPF 1
float
expf(float x)
{
  return (float) exp(x);
}
#endif

#ifndef HAVE_FABSF
#define HAVE_FABSF 1
float
fabsf(float x)
{
  return (float) fabs(x);
}
#endif

#ifndef HAVE_FLOORF
#define HAVE_FLOORF 1
float
floorf(float x)
{
  return (float) floor(x);
}
#endif

#ifndef HAVE_FMODF
#define HAVE_FMODF 1
float
fmodf (float x, float y)
{
  return (float) fmod (x, y);
}
#endif

#ifndef HAVE_FREXPF
#define HAVE_FREXPF 1
float
frexpf(float x, int *exp)
{
  return (float) frexp(x, exp);
}
#endif

#ifndef HAVE_HYPOTF
#define HAVE_HYPOTF 1
float
hypotf(float x, float y)
{
  return (float) hypot(x, y);
}
#endif

#ifndef HAVE_LOGF
#define HAVE_LOGF 1
float
logf(float x)
{
  return (float) log(x);
}
#endif

#ifndef HAVE_LOG10F
#define HAVE_LOG10F 1
float
log10f(float x)
{
  return (float) log10(x);
}
#endif

#ifndef HAVE_SCALBN
#define HAVE_SCALBN 1
double
scalbn(double x, int y)
{
#if (FLT_RADIX == 2) && defined(HAVE_LDEXP)
  return ldexp (x, y);
#else
  return x * pow(FLT_RADIX, y);
#endif
}
#endif

#ifndef HAVE_SCALBNF
#define HAVE_SCALBNF 1
float
scalbnf(float x, int y)
{
  return (float) scalbn(x, y);
}
#endif

#ifndef HAVE_SINF
#define HAVE_SINF 1
float
sinf(float x)
{
  return (float) sin(x);
}
#endif

#ifndef HAVE_SINHF
#define HAVE_SINHF 1
float
sinhf(float x)
{
  return (float) sinh(x);
}
#endif

#ifndef HAVE_SQRTF
#define HAVE_SQRTF 1
float
sqrtf(float x)
{
  return (float) sqrt(x);
}
#endif

#ifndef HAVE_TANF
#define HAVE_TANF 1
float
tanf(float x)
{
  return (float) tan(x);
}
#endif

#ifndef HAVE_TANHF
#define HAVE_TANHF 1
float
tanhf(float x)
{
  return (float) tanh(x);
}
#endif

#ifndef HAVE_TRUNC
#define HAVE_TRUNC 1
double
trunc(double x)
{
  if (!isfinite (x))
    return x;

  if (x < 0.0)
    return - floor (-x);
  else
    return floor (x);
}
#endif

#ifndef HAVE_TRUNCF
#define HAVE_TRUNCF 1
float
truncf(float x)
{
  return (float) trunc (x);
}
#endif

#ifndef HAVE_NEXTAFTERF
#define HAVE_NEXTAFTERF 1
/* This is a portable implementation of nextafterf that is intended to be
   independent of the floating point format or its in memory representation.
   This implementation works correctly with denormalized values.  */
float
nextafterf(float x, float y)
{
  /* This variable is marked volatile to avoid excess precision problems
     on some platforms, including IA-32.  */
  volatile float delta;
  float absx, denorm_min;

  if (isnan(x) || isnan(y))
    return x + y;
  if (x == y)
    return x;
  if (!isfinite (x))
    return x > 0 ? __FLT_MAX__ : - __FLT_MAX__;

  /* absx = fabsf (x);  */
  absx = (x < 0.0) ? -x : x;

  /* __FLT_DENORM_MIN__ is non-zero iff the target supports denormals.  */
  if (__FLT_DENORM_MIN__ == 0.0f)
    denorm_min = __FLT_MIN__;
  else
    denorm_min = __FLT_DENORM_MIN__;

  if (absx < __FLT_MIN__)
    delta = denorm_min;
  else
    {
      float frac;
      int exp;

      /* Discard the fraction from x.  */
      frac = frexpf (absx, &exp);
      delta = scalbnf (0.5f, exp);

      /* Scale x by the epsilon of the representation.  By rights we should
	 have been able to combine this with scalbnf, but some targets don't
	 get that correct with denormals.  */
      delta *= __FLT_EPSILON__;

      /* If we're going to be reducing the absolute value of X, and doing so
	 would reduce the exponent of X, then the delta to be applied is
	 one exponent smaller.  */
      if (frac == 0.5f && (y < x) == (x > 0))
	delta *= 0.5f;

      /* If that underflows to zero, then we're back to the minimum.  */
      if (delta == 0.0f)
	delta = denorm_min;
    }

  if (y < x)
    delta = -delta;

  return x + delta;
}
#endif


#if !defined(HAVE_POWF) || defined(HAVE_BROKEN_POWF)
#ifndef HAVE_POWF
#define HAVE_POWF 1
#endif
float
powf(float x, float y)
{
  return (float) pow(x, y);
}
#endif

/* Note that if fpclassify is not defined, then NaN is not handled */

/* Algorithm by Steven G. Kargl.  */

#if !defined(HAVE_ROUNDL)
#define HAVE_ROUNDL 1
#if defined(HAVE_CEILL)
/* Round to nearest integral value.  If the argument is halfway between two
   integral values then round away from zero.  */

long double
roundl(long double x)
{
   long double t;
   if (!isfinite (x))
     return (x);

   if (x >= 0.0)
    {
      t = ceill(x);
      if (t - x > 0.5)
	t -= 1.0;
      return (t);
    } 
   else 
    {
      t = ceill(-x);
      if (t + x > 0.5)
	t -= 1.0;
      return (-t);
    }
}
#else

/* Poor version of roundl for system that don't have ceill.  */
long double
roundl(long double x)
{
  if (x > DBL_MAX || x < -DBL_MAX)
    {
#ifdef HAVE_NEXTAFTERL
      static long double prechalf = nexafterl (0.5L, LDBL_MAX);
#else
      static long double prechalf = 0.5L;
#endif
      return (GFC_INTEGER_LARGEST) (x + (x > 0 ? prechalf : -prechalf));
    }
  else
    /* Use round().  */
    return round((double) x);
}

#endif
#endif

#ifndef HAVE_ROUND
#define HAVE_ROUND 1
/* Round to nearest integral value.  If the argument is halfway between two
   integral values then round away from zero.  */

double
round(double x)
{
   double t;
   if (!isfinite (x))
     return (x);

   if (x >= 0.0) 
    {
      t = ceil(x);
      if (t - x > 0.5)
	t -= 1.0;
      return (t);
    } 
   else 
    {
      t = ceil(-x);
      if (t + x > 0.5)
	t -= 1.0;
      return (-t);
    }
}
#endif

#ifndef HAVE_ROUNDF
#define HAVE_ROUNDF 1
/* Round to nearest integral value.  If the argument is halfway between two
   integral values then round away from zero.  */

float
roundf(float x)
{
   float t;
   if (!isfinite (x))
     return (x);

   if (x >= 0.0) 
    {
      t = ceilf(x);
      if (t - x > 0.5)
	t -= 1.0;
      return (t);
    } 
   else 
    {
      t = ceilf(-x);
      if (t + x > 0.5)
	t -= 1.0;
      return (-t);
    }
}
#endif


/* lround{f,,l} and llround{f,,l} functions.  */

#if !defined(HAVE_LROUNDF) && defined(HAVE_ROUNDF)
#define HAVE_LROUNDF 1
long int
lroundf (float x)
{
  return (long int) roundf (x);
}
#endif

#if !defined(HAVE_LROUND) && defined(HAVE_ROUND)
#define HAVE_LROUND 1
long int
lround (double x)
{
  return (long int) round (x);
}
#endif

#if !defined(HAVE_LROUNDL) && defined(HAVE_ROUNDL)
#define HAVE_LROUNDL 1
long int
lroundl (long double x)
{
  return (long long int) roundl (x);
}
#endif

#if !defined(HAVE_LLROUNDF) && defined(HAVE_ROUNDF)
#define HAVE_LLROUNDF 1
long long int
llroundf (float x)
{
  return (long long int) roundf (x);
}
#endif

#if !defined(HAVE_LLROUND) && defined(HAVE_ROUND)
#define HAVE_LLROUND 1
long long int
llround (double x)
{
  return (long long int) round (x);
}
#endif

#if !defined(HAVE_LLROUNDL) && defined(HAVE_ROUNDL)
#define HAVE_LLROUNDL 1
long long int
llroundl (long double x)
{
  return (long long int) roundl (x);
}
#endif


#ifndef HAVE_LOG10L
#define HAVE_LOG10L 1
/* log10 function for long double variables. The version provided here
   reduces the argument until it fits into a double, then use log10.  */
long double
log10l(long double x)
{
#if LDBL_MAX_EXP > DBL_MAX_EXP
  if (x > DBL_MAX)
    {
      double val;
      int p2_result = 0;
      if (x > 0x1p16383L) { p2_result += 16383; x /= 0x1p16383L; }
      if (x > 0x1p8191L) { p2_result += 8191; x /= 0x1p8191L; }
      if (x > 0x1p4095L) { p2_result += 4095; x /= 0x1p4095L; }
      if (x > 0x1p2047L) { p2_result += 2047; x /= 0x1p2047L; }
      if (x > 0x1p1023L) { p2_result += 1023; x /= 0x1p1023L; }
      val = log10 ((double) x);
      return (val + p2_result * .30102999566398119521373889472449302L);
    }
#endif
#if LDBL_MIN_EXP < DBL_MIN_EXP
  if (x < DBL_MIN)
    {
      double val;
      int p2_result = 0;
      if (x < 0x1p-16380L) { p2_result += 16380; x /= 0x1p-16380L; }
      if (x < 0x1p-8189L) { p2_result += 8189; x /= 0x1p-8189L; }
      if (x < 0x1p-4093L) { p2_result += 4093; x /= 0x1p-4093L; }
      if (x < 0x1p-2045L) { p2_result += 2045; x /= 0x1p-2045L; }
      if (x < 0x1p-1021L) { p2_result += 1021; x /= 0x1p-1021L; }
      val = fabs(log10 ((double) x));
      return (- val - p2_result * .30102999566398119521373889472449302L);
    }
#endif
    return log10 (x);
}
#endif


#ifndef HAVE_FLOORL
#define HAVE_FLOORL 1
long double
floorl (long double x)
{
  /* Zero, possibly signed.  */
  if (x == 0)
    return x;

  /* Large magnitude.  */
  if (x > DBL_MAX || x < (-DBL_MAX))
    return x;

  /* Small positive values.  */
  if (x >= 0 && x < DBL_MIN)
    return 0;

  /* Small negative values.  */
  if (x < 0 && x > (-DBL_MIN))
    return -1;

  return floor (x);
}
#endif


#ifndef HAVE_FMODL
#define HAVE_FMODL 1
long double
fmodl (long double x, long double y)
{
  if (y == 0.0L)
    return 0.0L;

  /* Need to check that the result has the same sign as x and magnitude
     less than the magnitude of y.  */
  return x - floorl (x / y) * y;
}
#endif


#if !defined(HAVE_CABSF)
#define HAVE_CABSF 1
float
cabsf (float complex z)
{
  return hypotf (REALPART (z), IMAGPART (z));
}
#endif

#if !defined(HAVE_CABS)
#define HAVE_CABS 1
double
cabs (double complex z)
{
  return hypot (REALPART (z), IMAGPART (z));
}
#endif

#if !defined(HAVE_CABSL) && defined(HAVE_HYPOTL)
#define HAVE_CABSL 1
long double
cabsl (long double complex z)
{
  return hypotl (REALPART (z), IMAGPART (z));
}
#endif


#if !defined(HAVE_CARGF)
#define HAVE_CARGF 1
float
cargf (float complex z)
{
  return atan2f (IMAGPART (z), REALPART (z));
}
#endif

#if !defined(HAVE_CARG)
#define HAVE_CARG 1
double
carg (double complex z)
{
  return atan2 (IMAGPART (z), REALPART (z));
}
#endif

#if !defined(HAVE_CARGL) && defined(HAVE_ATAN2L)
#define HAVE_CARGL 1
long double
cargl (long double complex z)
{
  return atan2l (IMAGPART (z), REALPART (z));
}
#endif


/* exp(z) = exp(a)*(cos(b) + i sin(b))  */
#if !defined(HAVE_CEXPF)
#define HAVE_CEXPF 1
float complex
cexpf (float complex z)
{
  float a, b;
  float complex v;

  a = REALPART (z);
  b = IMAGPART (z);
  COMPLEX_ASSIGN (v, cosf (b), sinf (b));
  return expf (a) * v;
}
#endif

#if !defined(HAVE_CEXP)
#define HAVE_CEXP 1
double complex
cexp (double complex z)
{
  double a, b;
  double complex v;

  a = REALPART (z);
  b = IMAGPART (z);
  COMPLEX_ASSIGN (v, cos (b), sin (b));
  return exp (a) * v;
}
#endif

#if !defined(HAVE_CEXPL) && defined(HAVE_COSL) && defined(HAVE_SINL) && defined(EXPL)
#define HAVE_CEXPL 1
long double complex
cexpl (long double complex z)
{
  long double a, b;
  long double complex v;

  a = REALPART (z);
  b = IMAGPART (z);
  COMPLEX_ASSIGN (v, cosl (b), sinl (b));
  return expl (a) * v;
}
#endif


/* log(z) = log (cabs(z)) + i*carg(z)  */
#if !defined(HAVE_CLOGF)
#define HAVE_CLOGF 1
float complex
clogf (float complex z)
{
  float complex v;

  COMPLEX_ASSIGN (v, logf (cabsf (z)), cargf (z));
  return v;
}
#endif

#if !defined(HAVE_CLOG)
#define HAVE_CLOG 1
double complex
clog (double complex z)
{
  double complex v;

  COMPLEX_ASSIGN (v, log (cabs (z)), carg (z));
  return v;
}
#endif

#if !defined(HAVE_CLOGL) && defined(HAVE_LOGL) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOGL 1
long double complex
clogl (long double complex z)
{
  long double complex v;

  COMPLEX_ASSIGN (v, logl (cabsl (z)), cargl (z));
  return v;
}
#endif


/* log10(z) = log10 (cabs(z)) + i*carg(z)  */
#if !defined(HAVE_CLOG10F)
#define HAVE_CLOG10F 1
float complex
clog10f (float complex z)
{
  float complex v;

  COMPLEX_ASSIGN (v, log10f (cabsf (z)), cargf (z));
  return v;
}
#endif

#if !defined(HAVE_CLOG10)
#define HAVE_CLOG10 1
double complex
clog10 (double complex z)
{
  double complex v;

  COMPLEX_ASSIGN (v, log10 (cabs (z)), carg (z));
  return v;
}
#endif

#if !defined(HAVE_CLOG10L) && defined(HAVE_LOG10L) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOG10L 1
long double complex
clog10l (long double complex z)
{
  long double complex v;

  COMPLEX_ASSIGN (v, log10l (cabsl (z)), cargl (z));
  return v;
}
#endif


/* pow(base, power) = cexp (power * clog (base))  */
#if !defined(HAVE_CPOWF)
#define HAVE_CPOWF 1
float complex
cpowf (float complex base, float complex power)
{
  return cexpf (power * clogf (base));
}
#endif

#if !defined(HAVE_CPOW)
#define HAVE_CPOW 1
double complex
cpow (double complex base, double complex power)
{
  return cexp (power * clog (base));
}
#endif

#if !defined(HAVE_CPOWL) && defined(HAVE_CEXPL) && defined(HAVE_CLOGL)
#define HAVE_CPOWL 1
long double complex
cpowl (long double complex base, long double complex power)
{
  return cexpl (power * clogl (base));
}
#endif


/* sqrt(z).  Algorithm pulled from glibc.  */
#if !defined(HAVE_CSQRTF)
#define HAVE_CSQRTF 1
float complex
csqrtf (float complex z)
{
  float re, im;
  float complex v;

  re = REALPART (z);
  im = IMAGPART (z);
  if (im == 0)
    {
      if (re < 0)
        {
          COMPLEX_ASSIGN (v, 0, copysignf (sqrtf (-re), im));
        }
      else
        {
          COMPLEX_ASSIGN (v, fabsf (sqrtf (re)), copysignf (0, im));
        }
    }
  else if (re == 0)
    {
      float r;

      r = sqrtf (0.5 * fabsf (im));

      COMPLEX_ASSIGN (v, r, copysignf (r, im));
    }
  else
    {
      float d, r, s;

      d = hypotf (re, im);
      /* Use the identity   2  Re res  Im res = Im x
         to avoid cancellation error in  d +/- Re x.  */
      if (re > 0)
        {
          r = sqrtf (0.5 * d + 0.5 * re);
          s = (0.5 * im) / r;
        }
      else
        {
          s = sqrtf (0.5 * d - 0.5 * re);
          r = fabsf ((0.5 * im) / s);
        }

      COMPLEX_ASSIGN (v, r, copysignf (s, im));
    }
  return v;
}
#endif

#if !defined(HAVE_CSQRT)
#define HAVE_CSQRT 1
double complex
csqrt (double complex z)
{
  double re, im;
  double complex v;

  re = REALPART (z);
  im = IMAGPART (z);
  if (im == 0)
    {
      if (re < 0)
        {
          COMPLEX_ASSIGN (v, 0, copysign (sqrt (-re), im));
        }
      else
        {
          COMPLEX_ASSIGN (v, fabs (sqrt (re)), copysign (0, im));
        }
    }
  else if (re == 0)
    {
      double r;

      r = sqrt (0.5 * fabs (im));

      COMPLEX_ASSIGN (v, r, copysign (r, im));
    }
  else
    {
      double d, r, s;

      d = hypot (re, im);
      /* Use the identity   2  Re res  Im res = Im x
         to avoid cancellation error in  d +/- Re x.  */
      if (re > 0)
        {
          r = sqrt (0.5 * d + 0.5 * re);
          s = (0.5 * im) / r;
        }
      else
        {
          s = sqrt (0.5 * d - 0.5 * re);
          r = fabs ((0.5 * im) / s);
        }

      COMPLEX_ASSIGN (v, r, copysign (s, im));
    }
  return v;
}
#endif

#if !defined(HAVE_CSQRTL) && defined(HAVE_COPYSIGNL) && defined(HAVE_SQRTL) && defined(HAVE_FABSL) && defined(HAVE_HYPOTL)
#define HAVE_CSQRTL 1
long double complex
csqrtl (long double complex z)
{
  long double re, im;
  long double complex v;

  re = REALPART (z);
  im = IMAGPART (z);
  if (im == 0)
    {
      if (re < 0)
        {
          COMPLEX_ASSIGN (v, 0, copysignl (sqrtl (-re), im));
        }
      else
        {
          COMPLEX_ASSIGN (v, fabsl (sqrtl (re)), copysignl (0, im));
        }
    }
  else if (re == 0)
    {
      long double r;

      r = sqrtl (0.5 * fabsl (im));

      COMPLEX_ASSIGN (v, copysignl (r, im), r);
    }
  else
    {
      long double d, r, s;

      d = hypotl (re, im);
      /* Use the identity   2  Re res  Im res = Im x
         to avoid cancellation error in  d +/- Re x.  */
      if (re > 0)
        {
          r = sqrtl (0.5 * d + 0.5 * re);
          s = (0.5 * im) / r;
        }
      else
        {
          s = sqrtl (0.5 * d - 0.5 * re);
          r = fabsl ((0.5 * im) / s);
        }

      COMPLEX_ASSIGN (v, r, copysignl (s, im));
    }
  return v;
}
#endif


/* sinh(a + i b) = sinh(a) cos(b) + i cosh(a) sin(b)  */
#if !defined(HAVE_CSINHF)
#define HAVE_CSINHF 1
float complex
csinhf (float complex a)
{
  float r, i;
  float complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinhf (r) * cosf (i), coshf (r) * sinf (i));
  return v;
}
#endif

#if !defined(HAVE_CSINH)
#define HAVE_CSINH 1
double complex
csinh (double complex a)
{
  double r, i;
  double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinh (r) * cos (i), cosh (r) * sin (i));
  return v;
}
#endif

#if !defined(HAVE_CSINHL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CSINHL 1
long double complex
csinhl (long double complex a)
{
  long double r, i;
  long double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinhl (r) * cosl (i), coshl (r) * sinl (i));
  return v;
}
#endif


/* cosh(a + i b) = cosh(a) cos(b) - i sinh(a) sin(b)  */
#if !defined(HAVE_CCOSHF)
#define HAVE_CCOSHF 1
float complex
ccoshf (float complex a)
{
  float r, i;
  float complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, coshf (r) * cosf (i), - (sinhf (r) * sinf (i)));
  return v;
}
#endif

#if !defined(HAVE_CCOSH)
#define HAVE_CCOSH 1
double complex
ccosh (double complex a)
{
  double r, i;
  double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, cosh (r) * cos (i), - (sinh (r) * sin (i)));
  return v;
}
#endif

#if !defined(HAVE_CCOSHL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CCOSHL 1
long double complex
ccoshl (long double complex a)
{
  long double r, i;
  long double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, coshl (r) * cosl (i), - (sinhl (r) * sinl (i)));
  return v;
}
#endif


/* tanh(a + i b) = (tanh(a) + i tan(b)) / (1 - i tanh(a) tan(b))  */
#if !defined(HAVE_CTANHF)
#define HAVE_CTANHF 1
float complex
ctanhf (float complex a)
{
  float rt, it;
  float complex n, d;

  rt = tanhf (REALPART (a));
  it = tanf (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}
#endif

#if !defined(HAVE_CTANH)
#define HAVE_CTANH 1
double complex
ctanh (double complex a)
{
  double rt, it;
  double complex n, d;

  rt = tanh (REALPART (a));
  it = tan (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}
#endif

#if !defined(HAVE_CTANHL) && defined(HAVE_TANL) && defined(HAVE_TANHL)
#define HAVE_CTANHL 1
long double complex
ctanhl (long double complex a)
{
  long double rt, it;
  long double complex n, d;

  rt = tanhl (REALPART (a));
  it = tanl (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}
#endif


/* sin(a + i b) = sin(a) cosh(b) + i cos(a) sinh(b)  */
#if !defined(HAVE_CSINF)
#define HAVE_CSINF 1
float complex
csinf (float complex a)
{
  float r, i;
  float complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinf (r) * coshf (i), cosf (r) * sinhf (i));
  return v;
}
#endif

#if !defined(HAVE_CSIN)
#define HAVE_CSIN 1
double complex
csin (double complex a)
{
  double r, i;
  double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sin (r) * cosh (i), cos (r) * sinh (i));
  return v;
}
#endif

#if !defined(HAVE_CSINL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CSINL 1
long double complex
csinl (long double complex a)
{
  long double r, i;
  long double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinl (r) * coshl (i), cosl (r) * sinhl (i));
  return v;
}
#endif


/* cos(a + i b) = cos(a) cosh(b) - i sin(a) sinh(b)  */
#if !defined(HAVE_CCOSF)
#define HAVE_CCOSF 1
float complex
ccosf (float complex a)
{
  float r, i;
  float complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, cosf (r) * coshf (i), - (sinf (r) * sinhf (i)));
  return v;
}
#endif

#if !defined(HAVE_CCOS)
#define HAVE_CCOS 1
double complex
ccos (double complex a)
{
  double r, i;
  double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, cos (r) * cosh (i), - (sin (r) * sinh (i)));
  return v;
}
#endif

#if !defined(HAVE_CCOSL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CCOSL 1
long double complex
ccosl (long double complex a)
{
  long double r, i;
  long double complex v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, cosl (r) * coshl (i), - (sinl (r) * sinhl (i)));
  return v;
}
#endif


/* tan(a + i b) = (tan(a) + i tanh(b)) / (1 - i tan(a) tanh(b))  */
#if !defined(HAVE_CTANF)
#define HAVE_CTANF 1
float complex
ctanf (float complex a)
{
  float rt, it;
  float complex n, d;

  rt = tanf (REALPART (a));
  it = tanhf (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}
#endif

#if !defined(HAVE_CTAN)
#define HAVE_CTAN 1
double complex
ctan (double complex a)
{
  double rt, it;
  double complex n, d;

  rt = tan (REALPART (a));
  it = tanh (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}
#endif

#if !defined(HAVE_CTANL) && defined(HAVE_TANL) && defined(HAVE_TANHL)
#define HAVE_CTANL 1
long double complex
ctanl (long double complex a)
{
  long double rt, it;
  long double complex n, d;

  rt = tanl (REALPART (a));
  it = tanhl (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}
#endif


#if !defined(HAVE_TGAMMA)
#define HAVE_TGAMMA 1

extern double tgamma (double); 

/* Fallback tgamma() function. Uses the algorithm from
   http://www.netlib.org/specfun/gamma and references therein.  */

#undef SQRTPI
#define SQRTPI 0.9189385332046727417803297

#undef PI
#define PI 3.1415926535897932384626434

double
tgamma (double x)
{
  int i, n, parity;
  double fact, res, sum, xden, xnum, y, y1, ysq, z;

  static double p[8] = {
    -1.71618513886549492533811e0,  2.47656508055759199108314e1,
    -3.79804256470945635097577e2,  6.29331155312818442661052e2,
     8.66966202790413211295064e2, -3.14512729688483675254357e4,
    -3.61444134186911729807069e4,  6.64561438202405440627855e4 };

  static double q[8] = {
    -3.08402300119738975254353e1,  3.15350626979604161529144e2,
    -1.01515636749021914166146e3, -3.10777167157231109440444e3,
     2.25381184209801510330112e4,  4.75584627752788110767815e3,
    -1.34659959864969306392456e5, -1.15132259675553483497211e5 };

  static double c[7] = {             -1.910444077728e-03,
     8.4171387781295e-04,            -5.952379913043012e-04,
     7.93650793500350248e-04,        -2.777777777777681622553e-03,
     8.333333333333333331554247e-02,  5.7083835261e-03 };

  static const double xminin = 2.23e-308;
  static const double xbig = 171.624;
  static const double xnan = __builtin_nan ("0x0"), xinf = __builtin_inf ();
  static double eps = 0;
  
  if (eps == 0)
    eps = nextafter(1., 2.) - 1.;

  parity = 0;
  fact = 1;
  n = 0;
  y = x;

  if (__builtin_isnan (x))
    return x;

  if (y <= 0)
    {
      y = -x;
      y1 = trunc(y);
      res = y - y1;

      if (res != 0)
	{
	  if (y1 != trunc(y1*0.5l)*2)
	    parity = 1;
	  fact = -PI / sin(PI*res);
	  y = y + 1;
	}
      else
	return x == 0 ? copysign (xinf, x) : xnan;
    }

  if (y < eps)
    {
      if (y >= xminin)
        res = 1 / y;
      else
	return xinf;
    }
  else if (y < 13)
    {
      y1 = y;
      if (y < 1)
	{
	  z = y;
	  y = y + 1;
	}
      else
	{
	  n = (int)y - 1;
	  y = y - n;
	  z = y - 1;
	}

      xnum = 0;
      xden = 1;
      for (i = 0; i < 8; i++)
	{
	  xnum = (xnum + p[i]) * z;
	  xden = xden * z + q[i];
	}

      res = xnum / xden + 1;

      if (y1 < y)
        res = res / y1;
      else if (y1 > y)
	for (i = 1; i <= n; i++)
	  {
	    res = res * y;
	    y = y + 1;
	  }
    }
  else
    {
      if (y < xbig)
	{
	  ysq = y * y;
	  sum = c[6];
	  for (i = 0; i < 6; i++)
	    sum = sum / ysq + c[i];

	  sum = sum/y - y + SQRTPI;
	  sum = sum + (y - 0.5) * log(y);
	  res = exp(sum);
	}
      else
	return x < 0 ? xnan : xinf;
    }

  if (parity)
    res = -res;
  if (fact != 1)
    res = fact / res;

  return res;
}
#endif



#if !defined(HAVE_LGAMMA)
#define HAVE_LGAMMA 1

extern double lgamma (double); 

/* Fallback lgamma() function. Uses the algorithm from
   http://www.netlib.org/specfun/algama and references therein, 
   except for negative arguments (where netlib would return +Inf)
   where we use the following identity:
       lgamma(y) = log(pi/(|y*sin(pi*y)|)) - lgamma(-y)
 */

double
lgamma (double y)
{

#undef SQRTPI
#define SQRTPI 0.9189385332046727417803297

#undef PI
#define PI 3.1415926535897932384626434

#define PNT68  0.6796875
#define D1    -0.5772156649015328605195174
#define D2     0.4227843350984671393993777
#define D4     1.791759469228055000094023

  static double p1[8] = {
              4.945235359296727046734888e0, 2.018112620856775083915565e2,
              2.290838373831346393026739e3, 1.131967205903380828685045e4,
              2.855724635671635335736389e4, 3.848496228443793359990269e4,
              2.637748787624195437963534e4, 7.225813979700288197698961e3 };
  static double q1[8] = {
              6.748212550303777196073036e1,  1.113332393857199323513008e3,
              7.738757056935398733233834e3,  2.763987074403340708898585e4,
              5.499310206226157329794414e4,  6.161122180066002127833352e4,
              3.635127591501940507276287e4,  8.785536302431013170870835e3 };
  static double p2[8] = {
              4.974607845568932035012064e0,  5.424138599891070494101986e2,
              1.550693864978364947665077e4,  1.847932904445632425417223e5,
              1.088204769468828767498470e6,  3.338152967987029735917223e6,
              5.106661678927352456275255e6,  3.074109054850539556250927e6 };
  static double q2[8] = {
              1.830328399370592604055942e2,  7.765049321445005871323047e3,
              1.331903827966074194402448e5,  1.136705821321969608938755e6,
              5.267964117437946917577538e6,  1.346701454311101692290052e7,
              1.782736530353274213975932e7,  9.533095591844353613395747e6 };
  static double p4[8] = {
              1.474502166059939948905062e4,  2.426813369486704502836312e6,
              1.214755574045093227939592e8,  2.663432449630976949898078e9,
              2.940378956634553899906876e10, 1.702665737765398868392998e11,
              4.926125793377430887588120e11, 5.606251856223951465078242e11 };
  static double q4[8] = {
              2.690530175870899333379843e3,  6.393885654300092398984238e5,
              4.135599930241388052042842e7,  1.120872109616147941376570e9,
              1.488613728678813811542398e10, 1.016803586272438228077304e11,
              3.417476345507377132798597e11, 4.463158187419713286462081e11 };
  static double  c[7] = {
             -1.910444077728e-03,            8.4171387781295e-04,
             -5.952379913043012e-04,         7.93650793500350248e-04,
             -2.777777777777681622553e-03,   8.333333333333333331554247e-02,
              5.7083835261e-03 };

  static double xbig = 2.55e305, xinf = __builtin_inf (), eps = 0,
		frtbig = 2.25e76;

  int i;
  double corr, res, xden, xm1, xm2, xm4, xnum, ysq;

  if (eps == 0)
    eps = __builtin_nextafter(1., 2.) - 1.;

  if ((y > 0) && (y <= xbig))
    {
      if (y <= eps)
	res = -log(y);
      else if (y <= 1.5)
	{
	  if (y < PNT68)
	    {
	      corr = -log(y);
	      xm1 = y;
	    }
	  else
	    {
	      corr = 0;
	      xm1 = (y - 0.5) - 0.5;
	    }

	  if ((y <= 0.5) || (y >= PNT68))
	    {
	      xden = 1;
	      xnum = 0;
	      for (i = 0; i < 8; i++)
		{
		  xnum = xnum*xm1 + p1[i];
		  xden = xden*xm1 + q1[i];
		}
	      res = corr + (xm1 * (D1 + xm1*(xnum/xden)));
	    }
	  else
	    {
	      xm2 = (y - 0.5) - 0.5;
	      xden = 1;
	      xnum = 0;
	      for (i = 0; i < 8; i++)
		{
		  xnum = xnum*xm2 + p2[i];
		  xden = xden*xm2 + q2[i];
		}
	      res = corr + xm2 * (D2 + xm2*(xnum/xden));
	    }
	}
      else if (y <= 4)
	{
	  xm2 = y - 2;
	  xden = 1;
	  xnum = 0;
	  for (i = 0; i < 8; i++)
	    {
	      xnum = xnum*xm2 + p2[i];
	      xden = xden*xm2 + q2[i];
	    }
	  res = xm2 * (D2 + xm2*(xnum/xden));
	}
      else if (y <= 12)
	{
	  xm4 = y - 4;
	  xden = -1;
	  xnum = 0;
	  for (i = 0; i < 8; i++)
	    {
	      xnum = xnum*xm4 + p4[i];
	      xden = xden*xm4 + q4[i];
	    }
	  res = D4 + xm4*(xnum/xden);
	}
      else
	{
	  res = 0;
	  if (y <= frtbig)
	    {
	      res = c[6];
	      ysq = y * y;
	      for (i = 0; i < 6; i++)
		res = res / ysq + c[i];
	    }
	  res = res/y;
	  corr = log(y);
	  res = res + SQRTPI - 0.5*corr;
	  res = res + y*(corr-1);
	}
    }
  else if (y < 0 && __builtin_floor (y) != y)
    {
      /* lgamma(y) = log(pi/(|y*sin(pi*y)|)) - lgamma(-y)
         For abs(y) very close to zero, we use a series expansion to
	 the first order in y to avoid overflow.  */
      if (y > -1.e-100)
        res = -2 * log (fabs (y)) - lgamma (-y);
      else
        res = log (PI / fabs (y * sin (PI * y))) - lgamma (-y);
    }
  else
    res = xinf;

  return res;
}
#endif


#if defined(HAVE_TGAMMA) && !defined(HAVE_TGAMMAF)
#define HAVE_TGAMMAF 1
extern float tgammaf (float);

float
tgammaf (float x)
{
  return (float) tgamma ((double) x);
}
#endif

#if defined(HAVE_LGAMMA) && !defined(HAVE_LGAMMAF)
#define HAVE_LGAMMAF 1
extern float lgammaf (float);

float
lgammaf (float x)
{
  return (float) lgamma ((double) x);
}
#endif
