// Stub definitions for long double math.

// Copyright (C) 2001-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <cmath>

// For targets which do not have support for long double versions,
// we use the following crude approximations. We keep saying that we'll do
// better later, but never do.

extern "C"
{
#ifndef _GLIBCXX_HAVE_FABSL
  long double
  fabsl(long double x)
  {
    return fabs((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ACOSL
  long double
  acosl(long double x)
  {
    return acos((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ASINL
  long double
  asinl(long double x)
  {
    return asin((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ATANL
  long double
  atanl(long double x)
  {
    return atan ((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_ATAN2L
  long double
  atan2l(long double x, long double y)
  {
    return atan2((double) x, (double) y);
  }
#endif

#ifndef _GLIBCXX_HAVE_CEILL
  long double
  ceill(long double x)
  {
    return ceil((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_COSL
  long double
  cosl(long double x)
  {
    return cos((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_COSHL
  long double
  coshl(long double x)
  {
    return cosh((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_EXPL
  long double
  expl(long double x)
  {
    return exp((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_FLOORL
  long double
  floorl(long double x)
  {
    return floor((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_FMODL
  long double
  fmodl(long double x, long double y)
  {
    return fmod((double) x, (double) y);
  }
#endif

#ifndef _GLIBCXX_HAVE_FREXPL
  long double
  frexpl(long double x, int *exp)
  {
    return frexp((double) x, exp);
  }
#endif

#ifndef _GLIBCXX_HAVE_SQRTL
  long double
  sqrtl(long double x)
  {
    return  sqrt((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_HYPOTL
  long double
  hypotl(long double x, long double y)
  {
    long double s = fabsl(x) + fabsl(y);
    if (s == 0.0L)
      return s;
    x /= s; y /= s;
    return s * sqrtl(x * x + y * y);
  }
#endif

#ifndef _GLIBCXX_HAVE_LDEXPL
  long double
  ldexpl(long double x, int exp)
  {
    return ldexp((double) x, exp);
  }
#endif

#ifndef _GLIBCXX_HAVE_LOGL
  long double
  logl(long double x)
  {
    return log((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_LOG10L
  long double
  log10l(long double x)
  {
    return log10((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_MODFL
  long double
  modfl(long double x, long double *iptr)
  {
    double result, temp;

    result = modf((double) x, &temp);
    *iptr = temp;
    return result;
  }
#endif

#ifndef _GLIBCXX_HAVE_POWL
  long double
  powl(long double x, long double y)
  {
    return pow((double) x, (double) y);
  }
#endif

#ifndef _GLIBCXX_HAVE_SINL
  long double
  sinl(long double x)
  {
    return sin((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_SINHL
  long double
  sinhl(long double x)
  {
    return sinh((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_TANL
  long double
  tanl(long double x)
  {
    return tan((double) x);
  }
#endif

#ifndef _GLIBCXX_HAVE_TANHL
  long double
  tanhl(long double x)
  {
    return tanh((double) x);
  }
#endif
} // extern "C"
