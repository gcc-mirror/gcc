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
# pragma GCC system_header
# include_next <math.h>
# include_next <stdlib.h>

# include <bits/c++config.h>

namespace std {

  //
  // int
  //

  inline int abs(int i)
  { return i > 0 ? i : -i; }

  inline long abs(long i)
  { return i > 0 ? i : -i; }
  
    //
    // float
    //

#if _GLIBCPP_HAVE___BUILTIN_FABSF
    inline float abs(float __x)
      { return __builtin_fabsf(__x); }
#elif _GLIBCPP_HAVE_FABSF
    inline float abs(float __x)
      { return ::fabsf(__x); }
#else
    inline float abs(float __x)
      { return ::fabs(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ACOSF
    inline float acos(float __x)
      { return ::acosf(__x); }
#else
    inline float acos(float __x)
      { return ::acos(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ASINF
    inline float asin(float __x)
      { return ::asinf(__x); }
#else
    inline float asin(float __x)
      { return ::asin(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATANF
    inline float atan(float __x)
      { return ::atanf(__x); }
#else
    inline float atan(float __x)
      { return ::atan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATAN2F
    inline float atan2(float __y, float __x)
      { return ::atan2f(__y, __x); }
#else
    inline float atan2(float __y, float __x)
      { return ::atan2(static_cast<double>(__y), static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_CEILF
    inline float ceil(float __x)
      { return ::ceilf(__x); }
#else
    inline float ceil(float __x)
      { return ::ceil(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE___BUILTIN_COSF
    inline float cos(float __x)
      { return __builtin_cosf(__x); }
#elif _GLIBCPP_HAVE_COSF
    inline float cos(float __x)
      { return ::cosf(__x); }
#else
    inline float cos(float __x)
      { return ::cos(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_COSHF
    inline float cosh(float __x)
      { return ::coshf(__x); }
#else
    inline float cosh(float __x)
      { return ::cosh(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_EXPF
    inline float exp(float __x)
      { return ::expf(__x); }
#else
    inline float exp(float __x)
      { return ::exp(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE___BUILTIN_FABSF
    inline float fabs(float __x)
      { return __builtin_fabsf(__x); }
#elif _GLIBCPP_HAVE_FABSF
    inline float fabs(float __x)
      { return ::fabsf(__x); }
#else
    inline float fabs(float __x)
      { return ::fabs(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_FLOORF
    inline float floor(float __x)
      { return ::floorf(__x); }
#else
    inline float floor(float __x)
      { return ::floor(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_FMODF
    inline float fmod(float __x, float __y)
      { return ::fmodf(__x, __y); }
#else
    inline float fmod(float __x, float __y)
      { return ::fmod(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

#if _GLIBCPP_HAVE_FREXPF
    inline float frexp(float __x, int* __exp)
      { return ::frexpf(__x, __exp); }
#else
    inline float frexp(float __x, int* __exp)
      { return ::frexp(__x, __exp); }
#endif

#if _GLIBCPP_HAVE_LDEXPF
    inline float ldexp(float __x, int __exp)
      { return ::ldexpf(__x, __exp); }
#else
    inline float ldexp(float __x, int __exp)
      { return ::ldexp(static_cast<double>(__x), __exp); }
#endif

#if _GLIBCPP_HAVE_LOGF
    inline float log(float __x)
      { return ::logf(__x); }
#else
    inline float log(float __x)
      { return ::log(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_LOG10F
    inline float log10(float __x)
      { return ::log10f(__x); }
#else
    inline float log10(float __x)
      { return ::log10(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_MODFF
    inline float modf(float __x, float* __iptr)
      { return ::modff(__x, __iptr); }
#else
    inline float modf(float __x, float* __iptr)
    {
       double __tmp;
       double __res = ::modf(static_cast<double>(__x), &__tmp);
       *__iptr = static_cast<float> (__tmp);
       return __res;
    }
#endif

#if _GLIBCPP_HAVE_POWF
    inline float pow(float __x, float __y)
      { return ::powf(__x, __y); }
#else
    inline float pow(float __x, float __y)
      { return ::pow(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

    float pow(float, int);

#if _GLIBCPP_HAVE___BUILTIN_SINF
    inline float sin(float __x)
      { return __builtin_sinf(__x); }
#elif _GLIBCPP_HAVE_SINF
    inline float sin(float __x)
      { return ::sinf(__x); }
#else
    inline float sin(float __x)
      { return ::sin(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_SINHF
    inline float sinh(float __x)
      { return ::sinhf(__x); }
#else
    inline float sinh(float __x)
      { return ::sinh(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE___BUILTIN_SQRTF
    inline float sqrt(float __x)
      { return __builtin_sqrtf(__x); }
#elif _GLIBCPP_HAVE_SQRTF
    inline float sqrt(float __x)
      { return ::sqrtf(__x); }
#else
    inline float sqrt(float __x)
      { return ::sqrt(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_TANF
    inline float tan(float __x)
      { return ::tanf(__x); }
#else
    inline float tan(float __x)
      { return ::tan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_TANHF
    inline float tanh(float __x)
      { return ::tanhf(__x); }
#else
    inline float tanh(float __x)
      { return ::tanh(static_cast<double>(__x)); }
#endif

    //
    // double
    //

#if _GLIBCPP_HAVE___BUILTIN_FABS
    inline double abs(double __x)
      { return __builtin_fabs(__x); }
#else
    inline double abs(double __x)
      { return ::fabs(__x); }
#endif

    inline double acos(double __x)
      { return ::acos(__x); }

    inline double asin(double __x)
      { return ::asin(__x); }

    inline double atan(double __x)
      { return ::atan(__x); }

    inline double atan2(double __y, double __x)
      { return ::atan2(__y, __x); }

    inline double ceil(double __x)
      { return ::ceil(__x); }

#if _GLIBCPP_HAVE___BUILTIN_COS
    inline double cos(double __x)
      { return __builtin_cos(__x); }
#else
    inline double cos(double __x)
      { return ::cos(__x); }
#endif

    inline double cosh(double __x)
      { return ::cosh(__x); }

    inline double exp(double __x)
      { return ::exp(__x); }


#if _GLIBCPP_HAVE___BUILTIN_FABS
    inline double fabs(double __x)
      { return __builtin_fabs(__x); }
#else
    inline double fabs(double __x)
      { return ::fabs(__x); }
#endif

    inline double floor(double __x)
      { return ::floor(__x); }

    inline double fmod(double __x, double __y)
      { return ::fmod(__x, __y); }

    inline double frexp(double __x, int* __exp)
      { return ::frexp(__x, __exp); }

    inline double ldexp(double __x, int __exp)
      { return ::ldexp(__x, __exp); }

    inline double log(double __x)
      { return ::log(__x); }

    inline double log10(double __x)
      { return ::log10(__x); }

    inline double modf(double __x, double* __iptr)
      { return ::modf(__x, __iptr); }

    inline double pow(double __x, double __y)
      { return ::pow(__x, __y); }

    double pow (double, int);

#if _GLIBCPP_HAVE___BUILTIN_SIN
    inline double sin(double __x)
      { return __builtin_sin(__x); }
#else
    inline double sin(double __x)
      { return ::sin(__x); }
#endif

    inline double sinh(double __x)
      { return ::sinh(__x); }

#if _GLIBCPP_HAVE___BUILTIN_SQRT
    inline double sqrt(double __x)
      { return __builtin_fsqrt(__x); }
#else
    inline double sqrt(double __x)
      { return ::sqrt(__x); }
#endif

    inline double tan(double __x)
      { return ::tan(__x); }

    inline double tanh(double __x)
      { return ::tanh(__x); }

    //
    // long double
    //
#if _GLIBCPP_HAVE___BUILTIN_FABSL
    inline long double abs(long double __x)
      { return __builtin_fabsl(__x); }
#elif _GLIBCPP_HAVE_FABSL
    inline long double abs(long double __x)
      { return ::fabsl(__x); }
#endif

#if _GLIBCPP_HAVE_ACOSL
    inline long double acos(long double __x)
      { return ::acosl(__x); }
#endif

#if _GLIBCPP_HAVE_ASINL
    inline long double asin(long double __x)
      { return ::asinl(__x); }
#endif

#if _GLIBCPP_HAVE_ATANL
    inline long double atan(long double __x)
      { return ::atanl(__x); }
#endif

#if _GLIBCPP_HAVE_ATAN2L
    inline long double atan2(long double __y, long double __x)
      { return ::atan2l(__y, __x); }
#endif

#if _GLIBCPP_HAVE_CEILL
    inline long double ceil(long double __x)
      { return ::ceill(__x); }
#endif

#if _GLIBCPP_HAVE___BUILTIN_COSL
    inline long double cos(long double __x)
      { return __builtin_cosl(__x); }
#elif _GLIBCPP_HAVE_COSL
    inline long double cos(long double __x)
      { return ::cosl(__x); }
#endif

#if _GLIBCPP_HAVE_COSHL
    inline long double cosh(long double __x)
      { return ::coshl(__x); }
#endif

#if _GLIBCPP_HAVE_EXPL
    inline long double exp(long double __x)
      { return ::expl(__x); }
#endif

#if _GLIBCPP_HAVE___BUILTIN_FABSL
    inline long double fabs(long double __x)
      { return __builtin_fabsl(__x); }
#elif _GLIBCPP_HAVE_FABSL
    inline long double fabs(long double __x)
      { return ::fabsl(__x); }
#endif

#if _GLIBCPP_HAVE_FLOORL
    inline long double floor(long double __x)
      { return ::floorl(__x); }
#endif

#if _GLIBCPP_HAVE_FMODL
    inline long double fmod(long double __x, long double __y)
      { return ::fmodl(__x, __y); }
#endif

#if _GLIBCPP_HAVE_FREXPL
    inline long double frexp(long double __x, int* __exp)
      { return ::frexpl(__x, __exp); }
#endif

#if _GLIBCPP_HAVE_LDEXPL
    inline long double ldexp(long double __x, int __exp)
      { return ::ldexpl(__x, __exp); }
#endif

#if _GLIBCPP_HAVE_LOGL
    inline long double log(long double __x)
      { return ::logl(__x); }
#endif

#if _GLIBCPP_HAVE_LOG10L
    inline long double log10(long double __x)
      { return ::log10l(__x); }
#endif

#if _GLIBCPP_HAVE_MODFL
    inline long double modf(long double __x, long double* __iptr)
      { return ::modfl(__x, __iptr); }
#endif

#if _GLIBCPP_HAVE_POWL
    inline long double pow(long double __x, long double __y)
      { return ::powl(__x, __y); }
#endif

    long double pow(long double, int);

#if _GLIBCPP_HAVE___BUILTIN_SINL
    inline long double sin(long double __x)
      { return __builtin_sinl(__x); }
#elif _GLIBCPP_HAVE_SINL
    inline long double sin(long double __x)
      { return ::sinl(__x); }
#endif

#if _GLIBCPP_HAVE_SINHL
    inline long double sinh(long double __x)
      { return ::sinhl(__x); }
#endif

#if _GLIBCPP_HAVE___BUILTIN_SQRTL
    inline long double sqrt(long double __x)
      { return __builtin_sqrtl(__x); }
#elif _GLIBCPP_HAVE_SQRTL
    inline long double sqrt(long double __x)
      { return ::sqrtl(__x); }
#endif

#if _GLIBCPP_HAVE_TANL
    inline long double tan(long double __x)
      { return ::tanl(__x); }
#endif

#if _GLIBCPP_HAVE_TANHL
    inline long double tanh(long double __x)
      { return ::tanhl(__x); }
#endif

} // std

#endif // _CPP_CMATH


