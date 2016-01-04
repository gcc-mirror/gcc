// Explicit instantiation file.

// Copyright (C) 1997-2016 Free Software Foundation, Inc.
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

//
// ISO C++ 14882:
//

#include <istream>
#include <iomanip>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template class basic_istream<char>;
  template istream& ws(istream&);
  template istream& operator>>(istream&, char&);
  template istream& operator>>(istream&, unsigned char&);
  template istream& operator>>(istream&, signed char&);
  template istream& operator>>(istream&, char*);
  template istream& operator>>(istream&, unsigned char*);
  template istream& operator>>(istream&, signed char*);

  template istream& operator>>(istream&, _Setfill<char>);
  template istream& operator>>(istream&, _Setiosflags);
  template istream& operator>>(istream&, _Resetiosflags);
  template istream& operator>>(istream&, _Setbase);
  template istream& operator>>(istream&, _Setprecision);
  template istream& operator>>(istream&, _Setw);

  template istream& istream::_M_extract(unsigned short&);
  template istream& istream::_M_extract(unsigned int&);  
  template istream& istream::_M_extract(long&);
  template istream& istream::_M_extract(unsigned long&);
  template istream& istream::_M_extract(bool&);
#ifdef _GLIBCXX_USE_LONG_LONG
  template istream& istream::_M_extract(long long&);
  template istream& istream::_M_extract(unsigned long long&);
#endif
  template istream& istream::_M_extract(float&);
  template istream& istream::_M_extract(double&);
  template istream& istream::_M_extract(long double&);
  template istream& istream::_M_extract(void*&);

#ifdef _GLIBCXX_USE_WCHAR_T
  template class basic_istream<wchar_t>;
  template wistream& ws(wistream&);
  template wistream& operator>>(wistream&, wchar_t&);
  template wistream& operator>>(wistream&, wchar_t*);

  template wistream& operator>>(wistream&, _Setfill<wchar_t>);
  template wistream& operator>>(wistream&, _Setiosflags);
  template wistream& operator>>(wistream&, _Resetiosflags);
  template wistream& operator>>(wistream&, _Setbase);
  template wistream& operator>>(wistream&, _Setprecision);
  template wistream& operator>>(wistream&, _Setw);

  template wistream& wistream::_M_extract(unsigned short&);
  template wistream& wistream::_M_extract(unsigned int&);  
  template wistream& wistream::_M_extract(long&);
  template wistream& wistream::_M_extract(unsigned long&);
  template wistream& wistream::_M_extract(bool&);
#ifdef _GLIBCXX_USE_LONG_LONG
  template wistream& wistream::_M_extract(long long&);
  template wistream& wistream::_M_extract(unsigned long long&);
#endif
  template wistream& wistream::_M_extract(float&);
  template wistream& wistream::_M_extract(double&);
  template wistream& wistream::_M_extract(long double&);
  template wistream& wistream::_M_extract(void*&);
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// XXX GLIBCXX_ABI Deprecated
#ifdef _GLIBCXX_LONG_DOUBLE_COMPAT

#define _GLIBCXX_LDBL_COMPAT(dbl, ldbl) \
  extern "C" void ldbl (void) __attribute__ ((alias (#dbl), weak))
_GLIBCXX_LDBL_COMPAT (_ZNSirsERd, _ZNSirsERe);
#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_LDBL_COMPAT (_ZNSt13basic_istreamIwSt11char_traitsIwEErsERd,
		      _ZNSt13basic_istreamIwSt11char_traitsIwEErsERe);
#endif
_GLIBCXX_LDBL_COMPAT (_ZNSi10_M_extractIdEERSiRT_,
		      _ZNSi10_M_extractIeEERSiRT_);
#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_LDBL_COMPAT (_ZNSt13basic_istreamIwSt11char_traitsIwEE10_M_extractIdEERS2_RT_,
		      _ZNSt13basic_istreamIwSt11char_traitsIwEE10_M_extractIeEERS2_RT_);
#endif

#endif // _GLIBCXX_LONG_DOUBLE_COMPAT
