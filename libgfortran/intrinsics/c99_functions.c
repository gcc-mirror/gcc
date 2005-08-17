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
#include "libgfortran.h"


#ifndef HAVE_ACOSF
float
acosf(float x)
{
  return (float) acos(x);
}
#endif

#ifndef HAVE_ASINF
float
asinf(float x)
{
  return (float) asin(x);
}
#endif

#ifndef HAVE_ATAN2F
float
atan2f(float y, float x)
{
  return (float) atan2(y, x);
}
#endif

#ifndef HAVE_ATANF
float
atanf(float x)
{
  return (float) atan(x);
}
#endif

#ifndef HAVE_CEILF
float
ceilf(float x)
{
  return (float) ceil(x);
}
#endif

#ifndef HAVE_COPYSIGNF
float
copysignf(float x, float y)
{
  return (float) copysign(x, y);
}
#endif

#ifndef HAVE_COSF
float
cosf(float x)
{
  return (float) cos(x);
}
#endif

#ifndef HAVE_COSHF
float
coshf(float x)
{
  return (float) cosh(x);
}
#endif

#ifndef HAVE_EXPF
float
expf(float x)
{
  return (float) exp(x);
}
#endif

#ifndef HAVE_FABSF
float
fabsf(float x)
{
  return (float) fabs(x);
}
#endif

#ifndef HAVE_FLOORF
float
floorf(float x)
{
  return (float) floor(x);
}
#endif

#ifndef HAVE_FREXPF
float
frexpf(float x, int *exp)
{
  return (float) frexp(x, exp);
}
#endif

#ifndef HAVE_HYPOTF
float
hypotf(float x, float y)
{
  return (float) hypot(x, y);
}
#endif

#ifndef HAVE_LOGF
float
logf(float x)
{
  return (float) log(x);
}
#endif

#ifndef HAVE_LOG10F
float
log10f(float x)
{
  return (float) log10(x);
}
#endif

#ifndef HAVE_SCALBN
double
scalbn(double x, int y)
{
  return x * pow(FLT_RADIX, y);
}
#endif

#ifndef HAVE_SCALBNF
float
scalbnf(float x, int y)
{
  return (float) scalbn(x, y);
}
#endif

#ifndef HAVE_SINF
float
sinf(float x)
{
  return (float) sin(x);
}
#endif

#ifndef HAVE_SINHF
float
sinhf(float x)
{
  return (float) sinh(x);
}
#endif

#ifndef HAVE_SQRTF
float
sqrtf(float x)
{
  return (float) sqrt(x);
}
#endif

#ifndef HAVE_TANF
float
tanf(float x)
{
  return (float) tan(x);
}
#endif

#ifndef HAVE_TANHF
float
tanhf(float x)
{
  return (float) tanh(x);
}
#endif

#ifndef HAVE_TRUNC
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
float
truncf(float x)
{
  return (float) trunc (x);
}
#endif

#ifndef HAVE_NEXTAFTERF
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
float
powf(float x, float y)
{
  return (float) pow(x, y);
}
#endif

/* Note that if fpclassify is not defined, then NaN is not handled */

/* Algorithm by Steven G. Kargl.  */

#ifndef HAVE_ROUND
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
