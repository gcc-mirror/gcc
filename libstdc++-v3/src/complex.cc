// The template and inlines for the -*- C++ -*- complex number classes.

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

#include <bits/std_cmath.h>
#include <bits/std_complex.h>

// This is a ISO C 9X header.
#include <math/mathconf.h>
#undef complex

#ifndef FLT
# define FLT double
# define FCT(name) ::name
#endif

namespace std
{
    
    template<>
    FLT
    abs(const complex<FLT>& __x)
    { return FCT(cabs)(__x._M_value); }

    template<>
    FLT
    arg(const complex<FLT>& __x)
    { return FCT(carg)(__x._M_value); }

    template<>
    complex<FLT>
    polar(const FLT& __rho, const FLT& __theta)
    {
#if defined _G_HAVE_SINCOS && !defined __osf__
      // Although sincos does exist on OSF3.2 and OSF4.0 we cannot use it
      // since the necessary types are not defined in the headers.
      FLT __sinx, __cosx;
      FCT(sincos)(__theta, &__sinx, &__cosx);
      return complex<FLT>(__rho * __cosx, __rho * __sinx);
#else
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
      complex<FLT> __tmpf(__rho * FCT(cos)(__theta),
                         __rho * FCT(sin)(__theta));
      return __tmpf;
#else
       return complex<FLT>(__rho * FCT(cos)(__theta),
                          __rho * FCT(sin)(__theta));
#endif
#endif
    }

    template<>
    complex<FLT>
    cos(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(ccos)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(ccos)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    cosh(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(ccosh)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(ccosh)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    exp(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(cexp)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(cexp)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    log(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(c_log)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(c_log)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    log10(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(clog10)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(clog10)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    pow(const complex<FLT>& __x, int __n)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(cexp) (__n * FCT(c_log)(__x._M_value)));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(cexp) (__n * FCT(c_log)(__x._M_value))); }
#endif


    template<>
    complex<FLT>
    pow(const complex<FLT>& __x, const FLT& __y)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(cexp) (__y * FCT(c_log)(__x._M_value)));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(cexp) (__y * FCT(c_log)(__x._M_value))); }
#endif

    template<>
    complex<FLT>
    pow(const complex<FLT>& __x, const complex<FLT>& __y)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(cpow)(__x._M_value, __y._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(cpow)(__x._M_value, __y._M_value)); }
#endif

    template<>
    complex<FLT>
    pow(const FLT& __x, const complex<FLT>& __y)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(cexp)(__y._M_value * FCT(log)(__x)));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(cexp)(__y._M_value * FCT(log)(__x))); }
#endif

    template<>
    complex<FLT>
    sin(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(csin)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(csin)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    sinh(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(csinh)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(csinh)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    sqrt(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(csqrt)(__x._M_value));
      return __tmpf;
    }
#else
    { return complex<FLT>(FCT(csqrt)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    tan(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(ctan)(__x._M_value));
      return __tmpf;
    }
#else
     { return complex<FLT>(FCT(ctan)(__x._M_value)); }
#endif

    template<>
    complex<FLT>
    tanh(const complex<FLT>& __x)
#if defined(_GLIBCPP_BUGGY_FLOAT_COMPLEX) \
      && defined(_GLIBCPP_FLOAT_SPECIALIZATION)
    {
      complex<FLT> __tmpf(FCT(ctanh)(__x._M_value));
      return __tmpf;
    }
#else
     { return complex<FLT>(FCT(ctanh)(__x._M_value)); }
#endif
    
} // namespace std













