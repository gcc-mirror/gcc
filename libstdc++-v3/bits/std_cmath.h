// -*- C++ -*- C math library.

// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 26.5  C library
//

// Note: this is not a conforming implementation.

#ifndef _CPP_CMATH
#define _CPP_CMATH 1
# include_next <math.h>
# include_next <stdlib.h>

#include <bits/c++config.h>

namespace std {

    inline long
    abs(long __l) { return ::labs(__l); }

    inline ldiv_t
    div(long __a, long __b) { return ::ldiv(__a, __b); }

    // double
    inline double
    abs(double __x) { return ::fabs(__x); }

    double
    pow(double, int);

    // float
    inline float
    ceil(float __x) { return ::ceil(static_cast<double>(__x)); }

    inline float
    floor(float __x) { return ::floor(static_cast<double>(__x)); }

    inline float
    fmod(float __x, float __y) 
    { return ::fmod(static_cast<double>(__x), static_cast<double>(__y)); }

    inline float
    frexp(float __x, int* __p) 
    { return ::frexp(static_cast<double>(__x), __p); }

    inline float
    ldexp(float __x, int __i) 
    { return ::ldexp(static_cast<double>(__x), __i); }

    inline float
    pow(float __x, float __y) 
    { return ::pow(static_cast<double>(__x), static_cast<double>(__y)); }

    float
    pow(float, int); 

#if _GLIBCPP_HAVE_ABSF
    inline float
    abs(float __x) { return ::absf(__x); }
#else
    inline float
    abs(float __x) { return ::fabs(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ACOSF
    inline float
    acos(float __x) { return ::acosf(__x); }
#else
    inline float
    acos(float __x) { return ::acos(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ASINF
    inline float
    asin(float __x) { return ::asinf(__x); }
#else
    inline float
    asin(float __x) { return ::asin(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATANF
    inline float
    atan(float __x) { return ::atanf(__x); }
#else
    inline float
    atan(float __x) { return ::atan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATAN2F
    inline float
    atan2(float __x, float __y) { return ::atan2f(__x, __y); }
#else
    inline float
    atan2(float __x, float __y) 
    { return ::atan2(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

#if _GLIBCPP_HAVE_TANF
    inline float
    tan(float __x) { return ::tanf(__x); }
#else
    inline float
    tan(float __x) { return ::tan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_SINHF
    inline float
    sinh(float __x) { return ::sinhf(__x); }
#else
    inline float
    sinh(float __x) { return ::sinh(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_TANHF
    inline float
    tanh(float __x) { return ::tanhf(__x); }
#else
    inline float
    tanh(float __x) { return ::tanh(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_COSHF
    inline float
    cosh(float __x) { return ::coshf(__x); }
#else
    inline float
    cosh(float __x) { return ::cosh(static_cast<double>(__x)); }
#endif
 
#if _GLIBCPP_HAVE_EXPF
    inline float
    exp(float __x) { return ::expf(__x); }
#else
    inline float
    exp(float __x) { return ::exp(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_LOGF
    inline float
    log(float __x) { return ::logf(__x); }
#else
    inline float
    log(float __x) { return ::log(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_LOG10F
    inline float
    log10(float __x) { return ::log10f(__x); }
#else
    inline float
    log10(float __x) { return ::log10(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_MODFF
    inline float
    modf(float __x, float* __p) { return ::modff(__x, __p); }
#else
    inline float
    modf(float __x, float* __p) 
    {
      double __tmp;
      double __res = ::modf(static_cast<double>(__x), &__tmp);
      *__p = static_cast<float> (__tmp);
      return __res;
    }
#endif

#if GLIBCPP_HAS_BUILTIN_SINF
    inline float
    sin(float __x) { return __builtin_sinf(__x); }
#elif _GLIBCPP_HAVE_SINF
    inline float
    sin(float __x) { return ::sinf(__x); }
#else
    inline float
    sin(float __x) { return ::sin(static_cast<double>(__x)); }
#endif

#if GLIBCPP_HAS_BUILTIN_COSF
    inline float
    cos(float __x) { return __builtin_cosf(__x); }
#elif _GLIBCPP_HAVE_COSF
    inline float
    cos(float __x) { return ::cosf(__x); }
#else
    inline float
    cos(float __x) { return ::cos(static_cast<double>(__x)); }
#endif

#if GLIBCPP_HAS_BUILTIN_FABSF
    inline float
    fabs(float __x) { return __builtin_fabsf(__x); }
#elif _GLIBCPP_HAVE_FABSF
    inline float
    fabs(float __x) { return ::fabsf(__x); }
#else
    inline float
    fabs(float __x) { return ::fabs(static_cast<double>(__x)); }
#endif

#if GLIBCPP_HAS_BUILTIN_SQRTF
    inline float
    sqrt(float __x) { return __builtin_sqrtf(__x); }
#elif _GLIBCPP_HAVE_SQRTF
    inline float
    sqrt(float __x) { return ::sqrtf(__x); }
#else
    inline float
    sqrt(float __x) { return ::fabs(static_cast<double>(__x)); }
#endif

    // XXX long double
    long double
    pow(long double, int);

} // std

#endif // _CPP_CMATH


