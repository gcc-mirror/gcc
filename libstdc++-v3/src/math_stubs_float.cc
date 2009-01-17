// Stub definitions for float math.

// Copyright (C) 2001, 2002, 2003, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cmath>

// For targets which do not have support for float versions,
// we use the following crude approximations. We keep saying that we'll do
// better later, but never do.

extern "C" 
{
#ifndef _GLIBCXX_HAVE_FABSF
  float
  fabsf(float x)
  {
    return (float) fabs(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ACOSF
  float
  acosf(float x)
  {
    return (float) acos(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ASINF
  float
  asinf(float x)
  {
    return (float) asin(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ATANF
  float
  atanf(float x)
  {
    return (float) atan(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ATAN2F
  float
  atan2f(float x, float y)
  {
    return (float) atan2(x, y);
  }
#endif

#ifndef _GLIBCXX_HAVE_CEILF
  float
  ceilf(float x)
  {
    return (float) ceil(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_COSF
  float
  cosf(float x)
  {
    return (float) cos(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_COSHF
  float
  coshf(float x)
  {
    return (float) cosh(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_EXPF
  float
  expf(float x)
  {
    return (float) exp(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_FLOORF
  float
  floorf(float x)
  {
    return (float) floor(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_FMODF
  float
  fmodf(float x, float y)
  {
    return (float) fmod(x, y);
  }
#endif

#ifndef _GLIBCXX_HAVE_FREXPF
  float
  frexpf(float x, int *exp)
  {
    return (float) frexp(x, exp);
  }
#endif

#ifndef _GLIBCXX_HAVE_SQRTF
  float
  sqrtf(float x)
  {
    return (float) sqrt(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_HYPOTF
  float
  hypotf(float x, float y)
  {
    float s = fabsf(x) + fabsf(y);
    if (s == 0.0F)
      return s;
    x /= s; y /= s;
    return s * sqrtf(x * x + y * y);
  }
#endif

#ifndef _GLIBCXX_HAVE_LDEXPF
  float
  ldexpf(float x, int exp)
  {
    return (float) ldexp(x, exp);
  }
#endif

#ifndef _GLIBCXX_HAVE_LOGF
  float
  logf(float x)
  {
    return (float) log(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_LOG10F
  float
  log10f(float x)
  {
    return (float) log10(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_MODFF
  float
  modff(float x, float *iptr)
  {
    double result, temp;

    result = modf(x, &temp);
    *iptr = (float) temp;
    return (float) result;
  }
#endif

#ifndef _GLIBCXX_HAVE_POWF
  float
  powf(float x, float y)
  {
    return (float) pow(x, y);
  }
#endif

#ifndef _GLIBCXX_HAVE_SINF
  float
  sinf(float x)
  {
    return (float) sin(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_SINHF
  float
  sinhf(float x)
  {
    return (float) sinh(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_TANF
  float
  tanf(float x)
  {
    return (float) tan(x);
  }
#endif

#ifndef _GLIBCXX_HAVE_TANHF
  float
  tanhf(float x)
  {
    return (float) tanh(x);
  }
#endif
} // extern "C"
