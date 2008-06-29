// String Conversions -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _STRING_CONVERSIONS_H
#define _STRING_CONVERSIONS_H 1

#pragma GCC system_header

#include <ext/numeric_traits.h>
#include <bits/functexcept.h>
#include <cstddef>
#include <cstdlib>
#include <cwchar>
#include <cstdio>
#include <cerrno>

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  // Helper for all the sto* functions.
  template<typename _TRet, typename _Ret = _TRet, typename _CharT,
	   typename... _Base>
    _Ret
    __stoa(_TRet (*__convf) (const _CharT*, _CharT**, _Base...),
	   const char* __name, const _CharT* __str, std::size_t* __idx,
	   _Base... __base)
    {
      _Ret __ret;

      _CharT* __endptr;
      errno = 0;
      const _TRet __tmp = __convf(__str, &__endptr, __base...);

      if (__endptr == __str)
	std::__throw_invalid_argument(__name);
      else if (errno == ERANGE
	       || (std::__are_same<_Ret, int>::__value
		   && (__tmp < __numeric_traits<int>::__min
		       || __tmp > __numeric_traits<int>::__max)))
	std::__throw_out_of_range(__name);
      else
	__ret = __tmp;

      if (__idx)
	*__idx = __endptr - __str;

      return __ret;
    }

  // Helper for the to_string / to_wstring functions.
  template<typename _String, typename _CharT = typename _String::value_type>
    _String
    __to_xstring(int (*__convf) (_CharT*, std::size_t, const _CharT*,
				 __builtin_va_list), std::size_t __n,
		 const _CharT* __fmt, ...)
    {
      // XXX Eventually the result will be constructed in place in
      // the C++0x string, likely with the help of internal hooks.
      _CharT* __s = static_cast<_CharT*>(__builtin_alloca(sizeof(_CharT)
							  * __n));

      __builtin_va_list __args;
      __builtin_va_start(__args, __fmt);

      const int __len = __convf(__s, __n, __fmt, __args);

      __builtin_va_end(__args);

      return _String(__s, __s + __len);
    }

_GLIBCXX_END_NAMESPACE

#endif // _STRING_CONVERSIONS_H
