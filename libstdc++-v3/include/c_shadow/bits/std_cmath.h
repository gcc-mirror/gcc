// -*- C++ -*- header wrapper.

// Copyright (C) 1997-1999, 2000, 2002 Free Software Foundation, Inc.
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

#ifndef _CPP_CMATH
#define _CPP_CMATH 1

# include <bits/c++config.h>

namespace _C_legacy {
  extern "C" {
#     define _IN_C_LEGACY_
#     pragma GCC system_header
#     include_next <math.h>
  }

#if _GLIBCPP_HAVE_ACOSF
  inline float 
  _CPP_acos_capture(float __x) { return acosf(__x); }
#else
  inline float 
  _CPP_acos_capture(float __x) { return acos(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ASINF
  inline float 
  _CPP_asin_capture(float __x) { return asinf(__x); }
#else
  inline float 
  _CPP_asin_capture(float __x) { return asin(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATANF
  inline float 
  _CPP_atan_capture(float __x) { return atanf(__x); }
#else
  inline float 
  _CPP_atan_capture(float __x) { return atan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATAN2F
  inline float 
  _CPP_atan2_capture(float __y, float __x) { return atan2f(__y, __x); }
#else
  inline float 
  _CPP_atan2_capture(float __y, float __x) 
  { return atan2(static_cast<double>(__y), static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_CEILF
  inline float 
  _CPP_ceil_capture(float __x) { return ceilf(__x); }
#else
  inline float 
  _CPP_ceil_capture(float __x) { return ceil(static_cast<double>(__x)); }
#endif

  inline float 
  _CPP_cos_capture(float __x) { return __builtin_cosf(__x); }

#if _GLIBCPP_HAVE_COSHF
  inline float 
  _CPP_cosh_capture(float __x) { return coshf(__x); }
#else
  inline float 
  _CPP_cosh_capture(float __x) { return cosh(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_EXPF
  inline float 
  _CPP_exp_capture(float __x) { return expf(__x); }
#else
  inline float 
  _CPP_exp_capture(float __x) { return exp(static_cast<double>(__x)); }
#endif

  inline float 
  _CPP_fabs_capture(float __x) { return __builtin_fabsf(__x); }

#if _GLIBCPP_HAVE_FLOORF
  inline float 
  _CPP_floor_capture(float __x) { return floorf(__x); }
#else
  inline float 
  _CPP_floor_capture(float __x) { return floor(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_FMODFF
  inline float 
  _CPP_fmod_capture(float __x, float __y) { return fmodf(__x, __y); }
#else
  inline float 
  _CPP_fmod_capture(float __x, float __y) 
  { return fmod(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

#if _GLIBCPP_HAVE_FREXPF
  inline float 
  _CPP_frexp_capture(float __x, int* __exp) { return frexpf(__x, __exp); }
#else
  inline float 
  _CPP_frexp_capture(float __x, int* __exp) { return frexp(__x, __exp); }
#endif

#if _GLIBCPP_HAVE_LDEXPF
  inline float 
  _CPP_ldexp_capture(float __x, int __exp) { return ldexpf(__x, __exp); }
#else
  inline float 
  _CPP_ldexp_capture(float __x, int __exp) 
  { return ldexp(static_cast<double>(__x), __exp); }
#endif

#if _GLIBCPP_HAVE_LOGF
  inline float 
  _CPP_log_capture(float __x) { return logf(__x); }
#else
  inline float 
  _CPP_log_capture(float __x) { return log(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_LOG10F
  inline float 
  _CPP_log10_capture(float __x) { return log10f(__x); }
#else
  inline float 
  _CPP_log10_capture(float __x) { return log10(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_MODFF
  inline float 
  _CPP_modf_capture(float __x, float* __iptr) { return modff(__x, __iptr); }
#else
  inline float 
  _CPP_modf_capture(float __x, float* __iptr)
  {
    double __tmp;
    double __res = _C_legacy::modf(static_cast<double>(__x), &__tmp);
    *__iptr = static_cast<float> (__tmp);
    return __res;
  }
#endif
  
#if _GLIBCPP_HAVE_POWF
  inline float 
  _CPP_pow_capture(float __x, float __y) { return powf(__x, __y); }
#else
  inline float 
  _CPP_pow_capture(float __x, float __y)
  { return pow(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

  float pow(float, int);

  inline float 
  _CPP_sin_capture(float __x) { return __builtin_sinf(__x); }

#if _GLIBCPP_HAVE_SINHF
  inline float 
  _CPP_sinh_capture(float __x) { return sinhf(__x); }
#else
  inline float 
  _CPP_sinh_capture(float __x) { return sinh(static_cast<double>(__x)); }
#endif

  inline float 
  _CPP_sqrt_capture(float __x) { return __builtin_sqrtf(__x); }

#if _GLIBCPP_HAVE_TANF
  inline float 
  _CPP_tan_capture(float __x) { return tanf(__x); }
#else
  inline float 
  _CPP_tan_capture(float __x) { return tan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_TANHF
  inline float 
  _CPP_tanh_capture(float __x) { return tanhf(__x); }
#else
  inline float 
  _CPP_tanh_capture(float __x) { return tanh(static_cast<double>(__x)); }
#endif


  inline double 
  _CPP_acos_capture(double __x) { return acos(__x); }

  inline double 
  _CPP_asin_capture(double __x) { return asin(__x); }

  inline double 
  _CPP_atan_capture(double __x) { return atan(__x); }

  inline double 
  _CPP_atan2_capture(double __y, double __x) { return atan2(__y, __x); }

  inline double 
  _CPP_ceil_capture(double __x) { return ceil(__x); }

  inline double 
  _CPP_cos_capture(double __x) { return __builtin_cos(__x); }

  inline double 
  _CPP_cosh_capture(double __x) { return cosh(__x); }

  inline double 
  _CPP_exp_capture(double __x) { return exp(__x); }

  inline double 
  _CPP_fabs_capture(double __x) { return __builtin_fabs(__x); }

  inline double 
  _CPP_floor_capture(double __x) { return floor(__x); }

  inline double 
  _CPP_fmod_capture(double __x, double __y) { return fmod(__x, __y); }

  inline double 
  _CPP_frexp_capture(double __x, int* __exp) { return frexp(__x, __exp); }

  inline double 
  _CPP_ldexp_capture(double __x, int __exp) { return ldexp(__x, __exp); }

  inline double 
  _CPP_log_capture(double __x) { return log(__x); }

  inline double 
  _CPP_log10_capture(double __x) { return log10(__x); }

  inline double 
  _CPP_modf_capture(double __x, double* __iptr) { return modf(__x, __iptr); }

  inline double 
  _CPP_pow_capture(double __x, double __y) { return pow(__x, __y); }

  inline double 
  _CPP_sin_capture(double __x) { return __builtin_sin(__x); }

  inline double 
  _CPP_sinh_capture(double __x) { return sinh(__x); }

  inline double 
  _CPP_sqrt_capture(double __x) { return __builtin_sqrt(__x); }

  inline double 
  _CPP_tan_capture(double __x) { return tan(__x); }

  inline double 
  _CPP_tanh_capture(double __x) { return tanh(__x); }

#if _GLIBCPP_HAVE_ACOSL
  inline long double 
  _CPP_acos_capture(long double __x) { return acosl(__x); }
#else
  inline long double 
  _CPP_acos_capture(long double __x) { return acos(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ASINL
  inline long double 
  _CPP_asin_capture(long double __x) { return asinl(__x); }
#else
  inline long double 
  _CPP_asin_capture(long double __x) { return asin(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATANL
  inline long double 
  _CPP_atan_capture(long double __x) { return atanl(__x); }
#else
  inline long double 
  _CPP_atan_capture(long double __x) { return atan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_ATAN2L
  inline long double 
  _CPP_atan2_capture(long double __y, long double __x)
  { return atan2l(__y, __x); }
#else
  inline long double 
  _CPP_atan2_capture(long double __y, long double __x) 
  { return atan2(static_cast<double>(__y), static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_CEILL
  inline long double 
  _CPP_ceil_capture(long double __x) { return ceill(__x); }
#else
  inline long double 
  _CPP_ceil_capture(long double __x) { return ceil(static_cast<double>(__x)); }
#endif

  inline long double 
  _CPP_cos_capture(long double __x) { return __builtin_cosl(__x); }

#if _GLIBCPP_HAVE_COSHL
  inline long double 
  _CPP_cosh_capture(long double __x) { return coshl(__x); }
#else
  inline long double 
  _CPP_cosh_capture(long double __x) { return cosh(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_EXPL
  inline long double 
  _CPP_exp_capture(long double __x) { return expl(__x); }
#else
  inline long double 
  _CPP_exp_capture(long double __x) { return exp(static_cast<double>(__x)); }
#endif

  inline long double 
  _CPP_fabs_capture(long double __x) { return __builtin_fabsl(__x); }

#if _GLIBCPP_HAVE_FLOORL
  inline long double 
  _CPP_floor_capture(long double __x) { return floorl(__x); }
#else
  inline long double 
  _CPP_floor_capture(long double __x) 
  { return floor(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_FMODL
  inline long double 
  _CPP_fmod_capture(long double __x, long double __y) 
  { return fmodl(__x, __y); }
#else
  inline long double 
  _CPP_fmod_capture(long double __x, long double __y)
  { return fmod(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

#if _GLIBCPP_HAVE_FREXPL
  inline long double 
  _CPP_frexp_capture(long double __x, int* __exp) 
  { return frexpl(__x, __exp); }
#else
  inline long double 
  _CPP_frexp_capture(long double __x, int* __exp)
  { return frexp(static_cast<double>(__x), __exp); }
#endif

#if _GLIBCPP_HAVE_LDEXPL
  inline long double 
  _CPP_ldexp_capture(long double __x, int __exp) { return ldexpl(__x, __exp); }
#else
  inline long double 
  _CPP_ldexp_capture(long double __x, int __exp)
  { return ldexp(static_cast<double>(__x), __exp); }
#endif

#if _GLIBCPP_HAVE_LOGL
  inline long double 
  _CPP_log_capture(long double __x) { return logl(__x); }
#else
  inline long double 
  _CPP_log_capture(long double __x) { return log(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_LOG10L
  inline long double 
  _CPP_log10_capture(long double __x) { return log10l(__x); }
#else
  inline long double 
  _CPP_log10_capture(long double __x) 
  { return log10(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_MODFL
  inline long double 
  _CPP_modf_capture(long double __x, long double* __iptr)
  { return modfl(__x, __iptr); }
#else
  inline long double 
  _CPP_modf_capture(long double __x, long double* __iptr)
  {
    double __tmp;
    double __res = _C_legacy::modf(static_cast<double>(__x), &__tmp);
    *__iptr = static_cast<long double> (__tmp);
    return __res;
  }
#endif

#if _GLIBCPP_HAVE_POWL
  inline long double 
  _CPP_pow_capture(long double __x, long double __y) { return powl(__x, __y); }
#else
  inline long double 
  _CPP_pow_capture(long double __x, long double __y)
  { return pow(static_cast<double>(__x), static_cast<double>(__y)); }
#endif

  inline long double 
  _CPP_sin_capture(long double __x) { return __builtin_sinl(__x); }

#if _GLIBCPP_HAVE_SINHL
  inline long double 
  _CPP_sinh_capture(long double __x) { return sinhl(__x); }
#else
  inline long double 
  _CPP_sinh_capture(long double __x) { return sinh(static_cast<double>(__x)); }
#endif

  inline long double 
  _CPP_sqrt_capture(long double __x) { return __builtin_sqrtl(__x); }

#if _GLIBCPP_HAVE_TANL
  inline long double 
  _CPP_tan_capture(long double __x) { return tanl(__x); }
#else
  inline long double 
  _CPP_tan_capture(long double __x) { return tan(static_cast<double>(__x)); }
#endif

#if _GLIBCPP_HAVE_TANHL
  inline long double 
  _CPP_tanh_capture(long double __x) { return tanhl(__x); }
#else
  inline long double 
  _CPP_tanh_capture(long double __x) { return tanh(static_cast<double>(__x)); }
#endif
} // namespace _C_legacy

# undef abs
# undef acos
# undef asin
# undef atan
# undef atan2
# undef ceil
# undef cos
# undef cosh
# undef exp
# undef fabs
# undef floor
# undef fmod
# undef frexp
# undef ldexp
# undef log
# undef log10
# undef modf
# undef pow
# undef sin
# undef sinh
# undef sqrt
# undef tan
# undef tanh

namespace std {
  inline float 
  abs(float __x) { return _C_legacy::_CPP_fabs_capture(__x); }

  inline float 
  acos(float __x) { return _C_legacy::_CPP_acos_capture(__x); }

  inline float 
  asin(float __x) { return _C_legacy::_CPP_asin_capture(__x); }

  inline float 
  atan(float __x) { return _C_legacy::_CPP_atan_capture(__x); }

  inline float 
  atan2(float __y, float __x) 
  { return _C_legacy::_CPP_atan2_capture(__y, __x); }

  inline float 
  ceil(float __x) { return _C_legacy::_CPP_ceil_capture(__x); }

  inline float 
  cos(float __x) { return _C_legacy::_CPP_cos_capture(__x); }

  inline float 
  cosh(float __x) { return _C_legacy::_CPP_cosh_capture(__x); }

  inline float 
  exp(float __x) { return _C_legacy::_CPP_exp_capture(__x); }

  inline float 
  fabs(float __x) { return _C_legacy::_CPP_fabs_capture(__x); }

  inline float 
  floor(float __x) { return _C_legacy::_CPP_floor_capture(__x); }

  inline float 
  fmod(float __x, float __y) 
  { return _C_legacy::_CPP_fmod_capture(__x, __y); }

  inline float 
  frexp(float __x, int* __exp) 
  { return _C_legacy::_CPP_frexp_capture(__x, __exp); }

  inline float 
  ldexp(float __x, int __exp)
  { return _C_legacy::_CPP_ldexp_capture(__x, __exp); }

  inline float 
  log(float __x) { return _C_legacy::_CPP_log_capture(__x); }

  inline float 
  log10(float __x) { return _C_legacy::_CPP_log10_capture(__x); }

  inline float 
  modf(float __x, float* __iptr) 
  { return _C_legacy::_CPP_modf_capture(__x, __iptr); }

  inline float 
  pow(float __x, float __y) { return _C_legacy::_CPP_pow_capture(__x, __y); }

  float 
  pow(float, int);

  inline float 
  sin(float __x) { return _C_legacy::_CPP_sin_capture(__x); }

  inline float 
  sinh(float __x) { return _C_legacy::_CPP_sinh_capture(__x); }

  inline float 
  sqrt(float __x) { return _C_legacy::_CPP_sqrt_capture(__x); }

  inline float 
  tan(float __x) { return _C_legacy::_CPP_tan_capture(__x); }

  inline float 
  tanh(float __x) { return _C_legacy::_CPP_tanh_capture(__x); }

  inline double 
  abs(double __x) { return _C_legacy::_CPP_fabs_capture(__x); }

  inline double 
  acos(double __x) { return _C_legacy::_CPP_acos_capture(__x); }

  inline double 
  asin(double __x) { return _C_legacy::_CPP_asin_capture(__x); }

  inline double 
  atan(double __x) { return _C_legacy::_CPP_atan_capture(__x); }

  inline double 
  atan2(double __y, double __x) 
  { return _C_legacy::_CPP_atan2_capture(__y, __x); }

  inline double 
  ceil(double __x) { return _C_legacy::_CPP_ceil_capture(__x); }

  inline double 
  cos(double __x) { return _C_legacy::_CPP_cos_capture(__x); }

  inline double 
  cosh(double __x) { return _C_legacy::_CPP_cosh_capture(__x); }

  inline double 
  exp(double __x) { return _C_legacy::_CPP_exp_capture(__x); }

  inline double 
  fabs(double __x) { return _C_legacy::_CPP_fabs_capture(__x); }

  inline double 
  floor(double __x) { return _C_legacy::_CPP_floor_capture(__x); }

  inline double 
  fmod(double __x, double __y) 
  { return _C_legacy::_CPP_fmod_capture(__x, __y); }

  inline double 
  frexp(double __x, int* __exp) 
  { return _C_legacy::_CPP_frexp_capture(__x, __exp); }

  inline double 
  ldexp(double __x, int __exp)
  { return _C_legacy::_CPP_ldexp_capture(__x, __exp); }

  inline double 
  log(double __x) { return _C_legacy::_CPP_log_capture(__x); }

  inline double 
  log10(double __x) { return _C_legacy::_CPP_log10_capture(__x); }

  inline double 
  modf(double __x, double* __iptr) 
  { return _C_legacy::_CPP_modf_capture(__x, __iptr); }

  inline double 
  pow(double __x, double __y) 
  { return _C_legacy::_CPP_pow_capture(__x, __y); }

  double 
  pow(double, int);

  inline double 
  sin(double __x) { return _C_legacy::_CPP_sin_capture(__x); }

  inline double 
  sinh(double __x) { return _C_legacy::_CPP_sinh_capture(__x); }

  inline double 
  sqrt(double __x) { return _C_legacy::_CPP_sqrt_capture(__x); }

  inline double 
  tan(double __x) { return _C_legacy::_CPP_tan_capture(__x); }

  inline double 
  tanh(double __x) { return _C_legacy::_CPP_tanh_capture(__x); }

  inline long double 
  abs(long double __x) { return _C_legacy::_CPP_fabs_capture(__x); }

  inline long double 
  acos(long double __x) { return _C_legacy::_CPP_acos_capture(__x); }

  inline long double 
  asin(long double __x) { return _C_legacy::_CPP_asin_capture(__x); }

  inline long double 
  atan(long double __x) { return _C_legacy::_CPP_atan_capture(__x); }

  inline long double 
  atan2(long double __y, long double __x) 
  { return _C_legacy::_CPP_atan2_capture(__y, __x); }

  inline long double 
  ceil(long double __x) { return _C_legacy::_CPP_ceil_capture(__x); }

  inline long double 
  cos(long double __x) { return _C_legacy::_CPP_cos_capture(__x); }

  inline long double 
  cosh(long double __x) { return _C_legacy::_CPP_cosh_capture(__x); }

  inline long double 
  exp(long double __x) { return _C_legacy::_CPP_exp_capture(__x); }

  inline long double 
  fabs(long double __x) { return _C_legacy::_CPP_fabs_capture(__x); }

  inline long double 
  floor(long double __x) { return _C_legacy::_CPP_floor_capture(__x); }

  inline long double 
  fmod(long double __x, long double __y) 
  { return _C_legacy::_CPP_fmod_capture(__x, __y); }

  inline long double 
  frexp(long double __x, int* __exp)
  { return _C_legacy::_CPP_frexp_capture(__x, __exp); }

  inline long double 
  ldexp(long double __x, int __exp)
  { return _C_legacy::_CPP_ldexp_capture(__x, __exp); }

  inline long double 
  log(long double __x) { return _C_legacy::_CPP_log_capture(__x); }

  inline long double 
  log10(long double __x) { return _C_legacy::_CPP_log10_capture(__x); }

  inline long double 
  modf(long double __x, long double* __iptr) 
  { return _C_legacy::_CPP_modf_capture(__x, __iptr); }

  inline long double 
  pow(long double __x, long double __y)
  { return _C_legacy::_CPP_pow_capture(__x, __y); }

  long double 
  pow(long double, int);

  inline long double 
  sin(long double __x) { return _C_legacy::_CPP_sin_capture(__x); }

  inline long double 
  sinh(long double __x) { return _C_legacy::_CPP_sinh_capture(__x); }

  inline long double 
  sqrt(long double __x) { return _C_legacy::_CPP_sqrt_capture(__x); }

  inline long double 
  tan(long double __x) { return _C_legacy::_CPP_tan_capture(__x); }

  inline long double 
  tanh(long double __x) { return _C_legacy::_CPP_tanh_capture(__x); }

} // namespace std

# undef _IN_C_LEGACY_

#endif
































