/* Implementation of various C99 functions 
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfortran; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

#ifndef HAVE_NEXTAFTERF
/* This is a portable implementation of nextafterf that is intended to be
   independent of the floating point format or its in memory representation.
   This implementation skips denormalized values, for example returning
   FLT_MIN as the next value after zero, as many target's frexpf, scalbnf
   and ldexpf functions don't work as expected with denormalized values.  */
float
nextafterf(float x, float y)
{
  int origexp, newexp;

  if (isnan(x) || isnan(y))
    return x+y;
  if (x == y)
    return x;

  if (x == 0.0f)
    return y > 0.0f ? FLT_MIN : -FLT_MIN;

  frexpf(x, &origexp);
  if (x >= 0.0)
    {
      if (y > x)
	{
	  if (x < FLT_MIN)
	    return FLT_MIN;
	  return x + scalbnf(FLT_EPSILON, origexp-1);
	}
      else if (x > FLT_MIN)
	{
	  float temp = x - scalbnf(FLT_EPSILON, origexp-1);
	  frexpf(temp, &newexp);
	  if (newexp == origexp)
	    return temp;
	  return x - scalbnf(FLT_EPSILON, origexp-2);
	}
      else
	return 0.0f;
    }
  else
    {
      if (y < x)
	{
	  if (x > -FLT_MIN)
	    return -FLT_MIN;
	  return x - scalbnf(FLT_EPSILON, origexp-1);
	}
      else if (x < -FLT_MIN)
	{
	  float temp = x + scalbnf(FLT_EPSILON, origexp-1);
	  frexpf(temp, &newexp);
	  if (newexp == origexp)
	    return temp;
	  return x + scalbnf(FLT_EPSILON, origexp-2);
	}
      else
	return 0.0f;
    }
}
#endif

/* Note that if HAVE_FPCLASSIFY is not defined, then NaN is not handled */

/* Algorithm by Steven G. Kargl.  */

#ifndef HAVE_ROUND
/* Round to nearest integral value.  If the argument is halfway between two
   integral values then round away from zero.  */

double
round(double x)
{
   double t;
#ifdef HAVE_FPCLASSIFY
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
#ifdef HAVE_FPCLASSIFY
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

