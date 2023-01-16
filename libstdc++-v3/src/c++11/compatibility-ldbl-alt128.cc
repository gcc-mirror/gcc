// Compatibility symbols for alternate 128-bit long-double format -*- C++ -*-

// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#define _GLIBCXX_USE_CXX11_ABI 0
#include <locale>

#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT

#if ! defined __LONG_DOUBLE_IBM128__ && ! defined __LONG_DOUBLE_IEEE128__
#error "compatibility-ldbl-alt128.cc must only be compiled for 128-bit long double"
#endif

#define C char
#define C_is_char
#include "locale-inst-numeric.h"
#include "locale-inst-monetary.h"
#include "compatibility-ldbl-facets-aliases.h"

#ifdef _GLIBCXX_USE_WCHAR_T
# undef C
# undef C_is_char
# define C wchar_t
# include "locale-inst-numeric.h"
# include "locale-inst-monetary.h"
# include "compatibility-ldbl-facets-aliases.h"
# undef C
#endif

#include <limits>
#include <functional>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // long double
  const bool numeric_limits<long double>::is_specialized;
  const int  numeric_limits<long double>::digits;
  const int  numeric_limits<long double>::digits10;
  const int  numeric_limits<long double>::max_digits10;
  const bool numeric_limits<long double>::is_signed;
  const bool numeric_limits<long double>::is_integer;
  const bool numeric_limits<long double>::is_exact;
  const int  numeric_limits<long double>::radix;
  const int  numeric_limits<long double>::min_exponent;
  const int  numeric_limits<long double>::min_exponent10;
  const int  numeric_limits<long double>::max_exponent;
  const int  numeric_limits<long double>::max_exponent10;
  const bool numeric_limits<long double>::has_infinity;
  const bool numeric_limits<long double>::has_quiet_NaN;
  const bool numeric_limits<long double>::has_signaling_NaN;
  const float_denorm_style numeric_limits<long double>::has_denorm;
  const bool numeric_limits<long double>::has_denorm_loss;
  const bool numeric_limits<long double>::is_iec559;
  const bool numeric_limits<long double>::is_bounded;
  const bool numeric_limits<long double>::is_modulo;
  const bool numeric_limits<long double>::traps;
  const bool numeric_limits<long double>::tinyness_before;
  const float_round_style numeric_limits<long double>::round_style;

  template<>
    void
    __convert_to_v(const char* __s, long double& __v, ios_base::iostate& __err,
		   const __c_locale& __cloc) throw()
    {
      char* __sanity;
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
      // Prefer strtold_l, as __strtold_l isn't prototyped in more recent
      // glibc versions.
      __v = strtold_l(__s, &__sanity, __cloc);
#else
      __v = __strtold_l(__s, &__sanity, __cloc);
#endif

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 23. Num_get overflow result.
      if (__sanity == __s || *__sanity != '\0')
	{
	  __v = 0.0l;
	  __err = ios_base::failbit;
	}
      else if (__v == numeric_limits<long double>::infinity())
	{
	  __v = numeric_limits<long double>::max();
	  __err = ios_base::failbit;
	}
      else if (__v == -numeric_limits<long double>::infinity())
	{
	  __v = -numeric_limits<long double>::max();
	  __err = ios_base::failbit;
	}
    }

  namespace
  {
    alignas(money_get<char>) char money_get_c[sizeof(money_get<char>)];
    alignas(money_put<char>) char money_put_c[sizeof(money_put<char>)];
    alignas(num_get<char>) char num_get_c[sizeof(num_get<char>)];
    alignas(num_put<char>) char num_put_c[sizeof(num_put<char>)];
#ifdef _GLIBCXX_USE_WCHAR_T
    alignas(money_get<wchar_t>) char money_get_w[sizeof(money_get<wchar_t>)];
    alignas(money_put<wchar_t>) char money_put_w[sizeof(money_put<wchar_t>)];
    alignas(num_get<wchar_t>) char num_get_w[sizeof(num_get<wchar_t>)];
    alignas(num_put<wchar_t>) char num_put_w[sizeof(num_put<wchar_t>)];
#endif
  }

  extern void
  __locale_Impl_init_extra_ldbl128(
      function<void(const locale::id*, const locale::facet*)>,
      bool);

  void
  locale::_Impl::_M_init_extra_ldbl128(bool classic)
  {
    if (classic)
      {
	_M_init_facet(new (&money_get_c) money_get<char>(1));
	_M_init_facet(new (&money_put_c) money_put<char>(1));
	_M_init_facet(new (&num_get_c) num_get<char>(1));
	_M_init_facet(new (&num_put_c) num_put<char>(1));
#ifdef _GLIBCXX_USE_WCHAR_T
	_M_init_facet(new (&money_get_w) money_get<wchar_t>(1));
	_M_init_facet(new (&money_put_w) money_put<wchar_t>(1));
	_M_init_facet(new (&num_get_w) num_get<wchar_t>(1));
	_M_init_facet(new (&num_put_w) num_put<wchar_t>(1));
#endif
      }
    else
      {
	_M_init_facet(new money_get<char>);
	_M_init_facet(new money_put<char>);
	_M_init_facet(new num_get<char>);
	_M_init_facet(new num_put<char>);
#ifdef _GLIBCXX_USE_WCHAR_T
	_M_init_facet(new money_get<wchar_t>);
	_M_init_facet(new money_put<wchar_t>);
	_M_init_facet(new num_get<wchar_t>);
	_M_init_facet(new num_put<wchar_t>);
#endif
      }

#if _GLIBCXX_USE_DUAL_ABI
    __locale_Impl_init_extra_ldbl128(
	[this](const locale::id* i, const facet* f) {
	    _M_install_facet(i, f);
	},
	classic);
#endif
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#include <istream>
#include <ostream>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  template istream& istream::operator>>(long double&);
  template istream& istream::_M_extract(long double&);
  template ostream& ostream::operator<<(long double);
  template ostream& ostream::_M_insert(long double);
#ifdef _GLIBCXX_USE_WCHAR_T
  template wistream& wistream::operator>>(long double&);
  template wistream& wistream::_M_extract(long double&);
  template wostream& wostream::operator<<(long double);
  template wostream& wostream::_M_insert(long double);
#endif
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#include <complex>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  template
    basic_istream<char, char_traits<char> >&
    operator>>(basic_istream<char, char_traits<char> >&,
	       complex<long double>&);
  template
    basic_ostream<char, char_traits<char> >&
    operator<<(basic_ostream<char, char_traits<char> >&,
               const complex<long double>&);
#ifdef _GLIBCXX_USE_WCHAR_T
  template
    basic_istream<wchar_t, char_traits<wchar_t> >&
    operator>>(basic_istream<wchar_t, char_traits<wchar_t> >&,
               complex<long double>&);
  template
    basic_ostream<wchar_t, char_traits<wchar_t> >&
    operator<<(basic_ostream<wchar_t, char_traits<wchar_t> >&,
               const complex<long double>&);
#endif
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#include <cmath>
#include <tr1/functional>

// For std::tr1::hash<long double>::operator()
#include "../c++98/hash-long-double-tr1-aux.cc"

// std::tr1::hash<long double>::operator()
// and std::hash<long double>::operator()
// are the same, no need to duplicate them.
#ifdef __LONG_DOUBLE_IBM128__
extern "C" size_t
_ZNKSt4hashIgEclEg (void)
  __attribute__((pure))
  __attribute__((alias ("_ZNKSt3tr14hashIgEclEg")));
#elif __LONG_DOUBLE_IEEE128__
extern "C" size_t
_ZNKSt4hashIu9__ieee128EclEu9__ieee128 (void)
  __attribute__((pure))
  __attribute__((alias ("_ZNKSt3tr14hashIu9__ieee128EclEu9__ieee128")));
#else
# error "Configuration error"
#endif

#if defined _GLIBCXX_USE_DUAL_ABI && defined __LONG_DOUBLE_IEEE128__
// PR libstdc++/105417
// The --with-long-double-abi=ibm build is missing some exports that are present in the
// --with-long-double-abi=ieee build. Those symbols never should have been exported at all,
// but now that they have been, they should be exported consistently by both ibm and ieee.

#define STR_(X) #X
#define STR(X) STR_(X)
#define JOIN_(X,Y) X ## Y
#define JOIN(X,Y) JOIN_(X,Y)

#define NUM_GET_TYPE(C, I) _ZNKSt17__gnu_cxx_ieee1287num_getI ## C ## St19istreambuf_iteratorI ## C ## St11char_traitsI ## C
#define FUNC_NAME(TAG, INT) EEE14_M_extract_int ## TAG ## I ## INT ## EES4_S4_S4_RSt8ios_baseRSt12_Ios_IostateRT_

// This defines __gnu_ieee128::num_get<CHAR>::_M_extract_int[abi:cxx11]<INT> as an alias for
// __gnu_ieee128::num_get<CHAR>::_M_extract_int<INT> (i.e. the same name without the abi-tag).
#define ALIAS(CHAR,MANGLED_CHAR,INT) extern "C" std::istreambuf_iterator<CHAR> \
  JOIN(NUM_GET_TYPE(MANGLED_CHAR,INT), FUNC_NAME(B5cxx11,INT)) (void) \
  __attribute__((alias (STR(NUM_GET_TYPE(MANGLED_CHAR,INT)) STR(FUNC_NAME(,INT)))))

ALIAS(char,c,j);
ALIAS(char,c,l);
ALIAS(char,c,m);
ALIAS(char,c,t);
ALIAS(char,c,x);
ALIAS(char,c,y);
#ifdef _GLIBCXX_USE_WCHAR_T
ALIAS(wchar_t,w,j);
ALIAS(wchar_t,w,l);
ALIAS(wchar_t,w,m);
ALIAS(wchar_t,w,t);
ALIAS(wchar_t,w,x);
ALIAS(wchar_t,w,y);
#endif
#endif

#endif
