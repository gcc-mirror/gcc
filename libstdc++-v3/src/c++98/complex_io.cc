// The template and inlines for the -*- C++ -*- complex number classes.

// Copyright (C) 2000-2013 Free Software Foundation, Inc.
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

#include <complex>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template
    basic_istream<char, char_traits<char> >&
    operator>>(basic_istream<char, char_traits<char> >&, complex<float>&);

  template
    basic_ostream<char, char_traits<char> >&
    operator<<(basic_ostream<char, char_traits<char> >&, 
	       const complex<float>&);

  template
    basic_istream<char, char_traits<char> >&
    operator>>(basic_istream<char, char_traits<char> >&, complex<double>&);

  template
    basic_ostream<char, char_traits<char> >&
    operator<<(basic_ostream<char, char_traits<char> >&, 
	       const complex<double>&);

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
               complex<float>&);

  template
    basic_ostream<wchar_t, char_traits<wchar_t> >&
    operator<<(basic_ostream<wchar_t, char_traits<wchar_t> >&,
               const complex<float>&);

  template
    basic_istream<wchar_t, char_traits<wchar_t> >&
    operator>>(basic_istream<wchar_t, char_traits<wchar_t> >&,
               complex<double>&);

  template
    basic_ostream<wchar_t, char_traits<wchar_t> >&
    operator<<(basic_ostream<wchar_t, char_traits<wchar_t> >&,
               const complex<double>&);

  template
    basic_istream<wchar_t, char_traits<wchar_t> >&
    operator>>(basic_istream<wchar_t, char_traits<wchar_t> >&,
               complex<long double>&);

  template
    basic_ostream<wchar_t, char_traits<wchar_t> >&
    operator<<(basic_ostream<wchar_t, char_traits<wchar_t> >&,
               const complex<long double>&);
#endif //_GLIBCXX_USE_WCHAR_T

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// XXX GLIBCXX_ABI Deprecated
#ifdef _GLIBCXX_LONG_DOUBLE_COMPAT

#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl), weak))

_GLIBCXX_LDBL_COMPAT (_ZStlsIdcSt11char_traitsIcEERSt13basic_ostreamIT0_T1_ES6_RKSt7complexIT_E,
		      _ZStlsIecSt11char_traitsIcEERSt13basic_ostreamIT0_T1_ES6_RKSt7complexIT_E);
#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_LDBL_COMPAT (_ZStlsIdwSt11char_traitsIwEERSt13basic_ostreamIT0_T1_ES6_RKSt7complexIT_E,
		      _ZStlsIewSt11char_traitsIwEERSt13basic_ostreamIT0_T1_ES6_RKSt7complexIT_E);
#endif
_GLIBCXX_LDBL_COMPAT (_ZStrsIdcSt11char_traitsIcEERSt13basic_istreamIT0_T1_ES6_RSt7complexIT_E,
		      _ZStrsIecSt11char_traitsIcEERSt13basic_istreamIT0_T1_ES6_RSt7complexIT_E);
#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_LDBL_COMPAT (_ZStrsIdwSt11char_traitsIwEERSt13basic_istreamIT0_T1_ES6_RSt7complexIT_E,
		      _ZStrsIewSt11char_traitsIwEERSt13basic_istreamIT0_T1_ES6_RSt7complexIT_E);
#endif

#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
