// -*- C++ -*- header wrapper.

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <math.h>
    }
    inline double _CPP_HUGE_VAL_capture()        { return HUGE_VAL; }
    inline double _CPP_acos_capture(double __x)  { return acos(__x); }
    inline double _CPP_asin_capture(double __x)  { return asin(__x); }
    inline double _CPP_atan_capture(double __x)  { return atan(__x); }
    inline double _CPP_atan2_capture(double __y, double __x) 
      { return atan2(__y,__x); }
    inline double _CPP_cos_capture(double __x)   { return cos(__x); }
    inline double _CPP_sin_capture(double __x)   { return sin(__x); }
    inline double _CPP_tan_capture(double __x)   { return tan(__x); }
    inline double _CPP_cosh_capture(double __x)  { return cosh(__x); }
    inline double _CPP_sinh_capture(double __x)  { return sinh(__x); }
    inline double _CPP_tanh_capture(double __x)  { return tanh(__x); }
    inline double _CPP_exp_capture(double __x)   { return exp(__x); }
    inline double _CPP_frexp_capture(double __x, int* __exp) 
      { return frexp(__x, __exp); }
    inline double _CPP_ldexp_capture(double __x, int __exp) 
      { return ldexp(__x, __exp); }
    inline double _CPP_log_capture(double __x)   { return log(__x); }
    inline double _CPP_log10_capture(double __x) { return log10(__x); }
    inline double _CPP_modf_capture(double __x, double* __iptr) 
      { return modf(__x, __iptr); }
    inline double _CPP_pow_capture(double __x, double __y) 
      { return pow(__x, __y); }
    inline double _CPP_sqrt_capture(double __x)  { return sqrt(__x); }
    inline double _CPP_ceil_capture(double __x)  { return ceil(__x); }
    inline double _CPP_fabs_capture(double __x)  { return fabs(__x); }
    inline double _CPP_floor_capture(double __x) { return floor(__x); }
    inline double _CPP_fmod_capture(double __x, double __y) 
      { return fmod(__x, __y); }

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

# undef HUGE_VAL
# define HUGE_VAL ::_C_Swamp::_CPP_HUGE_VAL_capture()
# undef acos
# undef asin
# undef atan
# undef atan2
# undef cos
# undef sin
# undef tan
# undef cosh
# undef sinh
# undef tanh
# undef exp
# undef frexp
# undef ldexp
# undef log
# undef log10
# undef modf
# undef pow
# undef sqrt
# undef ceil
# undef fabs
# undef floor
# undef fmod

  namespace _C_Swamp {
    namespace _C_Shadow {
      inline double acos(double __x)
        { return ::_C_Swamp::_CPP_acos_capture(__x); }
      inline double asin(double __x)
        { return ::_C_Swamp::_CPP_asin_capture(__x); }
      inline double atan(double __x)
        { return ::_C_Swamp::_CPP_atan_capture(__x); }
      inline double atan2(double __y, double __x)
      	{ return ::_C_Swamp::_CPP_atan2_capture(__y,__x); }
      inline double cos(double __x)
        { return ::_C_Swamp::_CPP_cos_capture(__x); }
      inline double sin(double __x)
        { return ::_C_Swamp::_CPP_sin_capture(__x); }
      inline double tan(double __x)
        { return ::_C_Swamp::_CPP_tan_capture(__x); }
      inline double cosh(double __x)
        { return ::_C_Swamp::_CPP_cosh_capture(__x); }
      inline double sinh(double __x)
        { return ::_C_Swamp::_CPP_sinh_capture(__x); }
      inline double tanh(double __x)
        { return ::_C_Swamp::_CPP_tanh_capture(__x); }
      inline double exp(double __x)
        { return ::_C_Swamp::_CPP_exp_capture(__x); }
      inline double frexp(double __x, int* __exp)
      	{ return ::_C_Swamp::_CPP_frexp_capture(__x, __exp); }
      inline double ldexp(double __x, int __exp)
      	{ return ::_C_Swamp::_CPP_ldexp_capture(__x, __exp); }
      inline double log(double __x)
        { return ::_C_Swamp::_CPP_log_capture(__x); }
      inline double log10(double __x)
        { return ::_C_Swamp::_CPP_log10_capture(__x); }
      inline double modf(double __x, double* __iptr)
      	{ return ::_C_Swamp::_CPP_modf_capture(__x, __iptr); }
      inline double pow(double __x, double __y)
      	{ return ::_C_Swamp::_CPP_pow_capture(__x, __y); }
      inline double sqrt(double __x)
        { return ::_C_Swamp::_CPP_sqrt_capture(__x); }
      inline double ceil(double __x)
        { return ::_C_Swamp::_CPP_ceil_capture(__x); }
      inline double fabs(double __x)
        { return ::_C_Swamp::_CPP_fabs_capture(__x); }
      inline double floor(double __x)
        { return ::_C_Swamp::_CPP_floor_capture(__x); }
      inline double fmod(double __x, double __y)
      	{ return ::_C_Swamp::_CPP_fmod_capture(__x, __y); }

    }
  }
  namespace std {

    // Adopt C names into std::
    using ::_C_Swamp::_C_Shadow::acos;
    using ::_C_Swamp::_C_Shadow::asin;
    using ::_C_Swamp::_C_Shadow::atan;
    using ::_C_Swamp::_C_Shadow::atan2;
    using ::_C_Swamp::_C_Shadow::cos;
    using ::_C_Swamp::_C_Shadow::sin;
    using ::_C_Swamp::_C_Shadow::tan;
    using ::_C_Swamp::_C_Shadow::cosh;
    using ::_C_Swamp::_C_Shadow::sinh;
    using ::_C_Swamp::_C_Shadow::tanh;
    using ::_C_Swamp::_C_Shadow::exp;
    using ::_C_Swamp::_C_Shadow::frexp;
    using ::_C_Swamp::_C_Shadow::ldexp;
    using ::_C_Swamp::_C_Shadow::log;
    using ::_C_Swamp::_C_Shadow::log10;
    using ::_C_Swamp::_C_Shadow::modf;
    using ::_C_Swamp::_C_Shadow::pow;
    using ::_C_Swamp::_C_Shadow::sqrt;
    using ::_C_Swamp::_C_Shadow::ceil;
    using ::_C_Swamp::_C_Shadow::fabs;
    using ::_C_Swamp::_C_Shadow::floor;
    using ::_C_Swamp::_C_Shadow::fmod;

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }

# undef _IN_C_SWAMP_

#endif
