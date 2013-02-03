// Copyright (C) 1997-2013 Free Software Foundation, Inc.
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

#include <locale>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Definitions for static const data members of time_base.
  template<> 
    const char*
    __timepunct_cache<char>::_S_timezones[14] =
    { 
      "GMT", "HST", "AKST", "PST", "MST", "CST", "EST", "AST", "NST", "CET", 
      "IST", "EET", "CST", "JST"  
    };
 
#ifdef _GLIBCXX_USE_WCHAR_T
  template<> 
    const wchar_t*
    __timepunct_cache<wchar_t>::_S_timezones[14] =
    { 
      L"GMT", L"HST", L"AKST", L"PST", L"MST", L"CST", L"EST", L"AST", 
      L"NST", L"CET", L"IST", L"EET", L"CST", L"JST"  
    };
#endif

  // Definitions for static const data members of money_base.
  const money_base::pattern 
  money_base::_S_default_pattern =  { {symbol, sign, none, value} };

  const char* money_base::_S_atoms = "-0123456789";

  const char* __num_base::_S_atoms_in = "-+xX0123456789abcdefABCDEF";
  const char* __num_base::_S_atoms_out ="-+xX0123456789abcdef0123456789ABCDEF";

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // According to the resolution of DR 231, about 22.2.2.2.2, p11,
  // "str.precision() is specified in the conversion specification".
  void
  __num_base::_S_format_float(const ios_base& __io, char* __fptr, 
			      char __mod) throw()
  {
    ios_base::fmtflags __flags = __io.flags();
    *__fptr++ = '%';
    // [22.2.2.2.2] Table 60
    if (__flags & ios_base::showpos)
      *__fptr++ = '+';
    if (__flags & ios_base::showpoint)
      *__fptr++ = '#';

    // As per DR 231: _always_, not only when 
    // __flags & ios_base::fixed || __prec > 0
    *__fptr++ = '.';
    *__fptr++ = '*';

    if (__mod)
      *__fptr++ = __mod;
    ios_base::fmtflags __fltfield = __flags & ios_base::floatfield;
    // [22.2.2.2.2] Table 58
    if (__fltfield == ios_base::fixed)
      *__fptr++ = 'f';
    else if (__fltfield == ios_base::scientific)
      *__fptr++ = (__flags & ios_base::uppercase) ? 'E' : 'e';
    else
      *__fptr++ = (__flags & ios_base::uppercase) ? 'G' : 'g';
    *__fptr = '\0';
  }

  bool
  __verify_grouping(const char* __grouping, size_t __grouping_size,
		    const string& __grouping_tmp) throw()
  {
    const size_t __n = __grouping_tmp.size() - 1;
    const size_t __min = std::min(__n, size_t(__grouping_size - 1));
    size_t __i = __n;
    bool __test = true;
    
    // Parsed number groupings have to match the
    // numpunct::grouping string exactly, starting at the
    // right-most point of the parsed sequence of elements ...
    for (size_t __j = 0; __j < __min && __test; --__i, ++__j)
      __test = __grouping_tmp[__i] == __grouping[__j];
    for (; __i && __test; --__i)
      __test = __grouping_tmp[__i] == __grouping[__min];
    // ... but the first parsed grouping can be <= numpunct
    // grouping (only do the check if the numpunct char is > 0
    // because <= 0 means any size is ok).
    if (static_cast<signed char>(__grouping[__min]) > 0
	&& __grouping[__min] != __gnu_cxx::__numeric_traits<char>::__max)
      __test &= __grouping_tmp[0] <= __grouping[__min];
    return __test;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
