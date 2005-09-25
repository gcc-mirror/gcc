/* Implementation of various C99 functions 
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <sys/types.h>
#include <float.h>
#include <math.h>

#define C99_PROTOS_H WE_DONT_WANT_PROTOS_NOW
#include "libgfortran.h"


#ifndef HAVE_ACOSF
#define HAVE_ACOSF
float
acosf(float x)
{
  return (float) acos(x);
}
#endif

#ifndef HAVE_ASINF
#define HAVE_ASINF
float
asinf(float x)
{
  return (float) asin(x);
}
#endif

#ifndef HAVE_ATAN2F
#define HAVE_ATAN2F
float
atan2f(float y, float x)
{
  return (float) atan2(y, x);
}
#endif

#ifndef HAVE_ATANF
#define HAVE_ATANF
float
atanf(float x)
{
  return (float) atan(x);
}
#endif

#ifndef HAVE_CEILF
#define HAVE_CEILF
float
ceilf(float x)
{
  return (float) ceil(x);
}
#endif

#ifndef HAVE_COPYSIGNF
#define HAVE_COPYSIGNF
float
copysignf(float x, float y)
{
  return (float) copysign(x, y);
}
#endif

#ifndef HAVE_COSF
#define HAVE_COSF
float
cosf(float x)
{
  return (float) cos(x);
}
#endif

#ifndef HAVE_COSHF
#define HAVE_COSHF
float
coshf(float x)
{
  return (float) cosh(x);
}
#endif

#ifndef HAVE_EXPF
#define HAVE_EXPF
float
expf(float x)
{
  return (float) exp(x);
}
#endif

#ifndef HAVE_FABSF
#define HAVE_FABSF
float
fabsf(float x)
{
  return (float) fabs(x);
}
#endif

#ifndef HAVE_FLOORF
#define HAVE_FLOORF
float
floorf(float x)
{
  return (float) floor(x);
}
#endif

#ifndef HAVE_FREXPF
#define HAVE_FREXPF
float
frexpf(float x, int *exp)
{
  return (float) frexp(x, exp);
}
#endif

#ifndef HAVE_HYPOTF
#define HAVE_HYPOTF
float
hypotf(float x, float y)
{
  return (float) hypot(x, y);
}
#endif

#ifndef HAVE_LOGF
#define HAVE_LOGF
float
logf(float x)
{
  return (float) log(x);
}
#endif

#ifndef HAVE_LOG10F
#define HAVE_LOG10F
float
log10f(float x)
{
  return (float) log10(x);
}
#endif

#ifndef HAVE_SCALBN
#define HAVE_SCALBN
double
scalbn(double x, int y)
{
  return x * pow(FLT_RADIX, y);
}
#endif

#ifndef HAVE_SCALBNF
#define HAVE_SCALBNF
float
scalbnf(float x, int y)
{
  return (float) scalbn(x, y);
}
#endif

#ifndef HAVE_SINF
#define HAVE_SINF
float
sinf(float x)
{
  return (float) sin(x);
}
#endif

#ifndef HAVE_SINHF
#define HAVE_SINHF
float
sinhf(float x)
{
  return (float) sinh(x);
}
#endif

#ifndef HAVE_SQRTF
#define HAVE_SQRTF
float
sqrtf(float x)
{
  return (float) sqrt(x);
}
#endif

#ifndef HAVE_TANF
#define HAVE_TANF
float
tanf(float x)
{
  return (float) tan(x);
}
#endif

#ifndef HAVE_TANHF
#define HAVE_TANHF
float
tanhf(float x)
{
  return (float) tanh(x);
}
#endif

#ifndef HAVE_TRUNC
#define HAVE_TRUNC
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
#define HAVE_TRUNCF
float
truncf(float x)
{
  return (float) trunc (x);
}
#endif

#ifndef HAVE_NEXTAFTERF
#define HAVE_NEXTAFTERF
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


#ifndef HAVE_POWF
#define HAVE_POWF
float
powf(float x, float y)
{
  return (float) pow(x, y);
}
#endif

/* Note that if fpclassify is not defined, then NaN is not handled */

/* Algorithm by Steven G. Kargl.  */

#ifndef HAVE_ROUND
#define HAVE_ROUND
/* Round to nearest integral value.  If the argument is halfway between two
   integral values then round away from zero.  */

double
round(double x)
{
   double t;
#if defined(fpclassify)
   int i;
   i = fpclassify(x);
   if (i == FP_INFINITE || i == FP_NAN)
     return (x);
#endif

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
#define HAVE_ROUNDF
/* Round to nearest integral value.  If the argument is halfway between two
   integral values then round away from zero.  */

float
roundf(float x)
{
   float t;
#if defined(fpclassify)
   int i;

   i = fpclassify(x);
   if (i == FP_INFINITE || i == FP_NAN)
     return (x);
#endif

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

#ifndef HAVE_LOG10L
#define HAVE_LOG10L
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


#if !defined(HAVE_CABSF)
#define HAVE_CABSF
float
cabsf (float complex z)
{
  return hypotf (REALPART (z), IMAGPART (z));
}
#endif

#if !defined(HAVE_CABS)
#define HAVE_CABS
double
cabs (double complex z)
{
  return hypot (REALPART (z), IMAGPART (z));
}
#endif

#if !defined(HAVE_CABSL) && defined(HAVE_HYPOTL)
#define HAVE_CABSL
long double
cabsl (long double complex z)
{
  return hypotl (REALPART (z), IMAGPART (z));
}
#endif


#if !defined(HAVE_CARGF)
#define HAVE_CARGF
float
cargf (float complex z)
{
  return atan2f (IMAGPART (z), REALPART (z));
}
#endif

#if !defined(HAVE_CARG)
#define HAVE_CARG
double
carg (double complex z)
{
  return atan2 (IMAGPART (z), REALPART (z));
}
#endif

#if !defined(HAVE_CARGL) && defined(HAVE_ATAN2L)
#define HAVE_CARGL
long double
cargl (long double complex z)
{
  return atan2l (IMAGPART (z), REALPART (z));
}
#endif


/* exp(z) = exp(a)*(cos(b) + i sin(b))  */
#if !defined(HAVE_CEXPF)
#define HAVE_CEXPF
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
#define HAVE_CEXP
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
#define HAVE_CEXPL
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
#define HAVE_CLOGF
float complex
clogf (float complex z)
{
  float complex v;

  COMPLEX_ASSIGN (v, logf (cabsf (z)), cargf (z));
  return v;
}
#endif

#if !defined(HAVE_CLOG)
#define HAVE_CLOG
double complex
clog (double complex z)
{
  double complex v;

  COMPLEX_ASSIGN (v, log (cabs (z)), carg (z));
  return v;
}
#endif

#if !defined(HAVE_CLOGL) && defined(HAVE_LOGL) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOGL
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
#define HAVE_CLOG10F
float complex
clog10f (float complex z)
{
  float complex v;

  COMPLEX_ASSIGN (v, log10f (cabsf (z)), cargf (z));
  return v;
}
#endif

#if !defined(HAVE_CLOG10)
#define HAVE_CLOG10
double complex
clog10 (double complex z)
{
  double complex v;

  COMPLEX_ASSIGN (v, log10 (cabs (z)), carg (z));
  return v;
}
#endif

#if !defined(HAVE_CLOG10L) && defined(HAVE_LOG10L) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOG10L
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
#define HAVE_CPOWF
float complex
cpowf (float complex base, float complex power)
{
  return cexpf (power * clogf (base));
}
#endif

#if !defined(HAVE_CPOW)
#define HAVE_CPOW
double complex
cpow (double complex base, double complex power)
{
  return cexp (power * clog (base));
}
#endif

#if !defined(HAVE_CPOWL) && defined(HAVE_CEXPL) && defined(HAVE_CLOGL)
#define HAVE_CPOWL
long double complex
cpowl (long double complex base, long double complex power)
{
  return cexpl (power * clogl (base));
}
#endif


/* sqrt(z).  Algorithm pulled from glibc.  */
#if !defined(HAVE_CSQRTF)
#define HAVE_CSQRTF
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

      COMPLEX_ASSIGN (v, copysignf (r, im), r);
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
#define HAVE_CSQRT
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

      COMPLEX_ASSIGN (v, copysign (r, im), r);
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
#define HAVE_CSQRTL
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
#define HAVE_CSINHF
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
#define HAVE_CSINH
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
#define HAVE_CSINHL
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
#define HAVE_CCOSHF
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
#define HAVE_CCOSH
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
#define HAVE_CCOSHL
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
#define HAVE_CTANHF
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
#define HAVE_CTANH
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
#define HAVE_CTANHL
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
#define HAVE_CSINF
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
#define HAVE_CSIN
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
#define HAVE_CSINL
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
#define HAVE_CCOSF
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
#define HAVE_CCOS
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
#define HAVE_CCOSL
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
#define HAVE_CTANF
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
#define HAVE_CTAN
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
#define HAVE_CTANL
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

