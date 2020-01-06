// Locale support -*- C++ -*-

// Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

/** @file ctype_configure_char.cc */

//
// ISO C++ 14882: 22.1  Locales
//

#include <locale>
#include <cstdlib>
#include <cstring>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

// Information as gleaned from /usr/include/ctype.h

#if _GLIBCXX_C_LOCALE_GNU
  const ctype_base::mask*
  ctype<char>::classic_table() throw()
  { return _S_get_c_locale()->__ctype_b; }
#else
  const ctype_base::mask*
  ctype<char>::classic_table() throw()
  {
    const ctype_base::mask* __ret;
    char* __old = setlocale(LC_CTYPE, NULL);
    char* __sav = NULL;
    if (__builtin_strcmp(__old, "C"))
      {
	const size_t __len = __builtin_strlen(__old) + 1;
	__sav = new char[__len];
	__builtin_memcpy(__sav, __old, __len);
	setlocale(LC_CTYPE, "C");
      }
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __ret = *__ctype_b_loc();
#else
    __ret = __ctype_b;
#endif
    if (__sav)
      {
	setlocale(LC_CTYPE, __sav);
	delete [] __sav;
      }
    return __ret;
  }
#endif

#if _GLIBCXX_C_LOCALE_GNU
  ctype<char>::ctype(__c_locale __cloc, const mask* __table, bool __del,
		     size_t __refs)
  : facet(__refs), _M_c_locale_ctype(_S_clone_c_locale(__cloc)),
  _M_del(__table != 0 && __del),
  _M_toupper(_M_c_locale_ctype->__ctype_toupper),
  _M_tolower(_M_c_locale_ctype->__ctype_tolower),
  _M_table(__table ? __table : _M_c_locale_ctype->__ctype_b),
  _M_widen_ok(0), _M_narrow_ok(0)
  {
    __builtin_memset(_M_widen, 0, sizeof(_M_widen));
    __builtin_memset(_M_narrow, 0, sizeof(_M_narrow));
  }
#else
  ctype<char>::ctype(__c_locale, const mask* __table, bool __del,
		     size_t __refs)
  : facet(__refs), _M_c_locale_ctype(_S_get_c_locale()),
  _M_del(__table != 0 && __del), _M_widen_ok(0), _M_narrow_ok(0)
  {
    char* __old = setlocale(LC_CTYPE, NULL);
    char* __sav = NULL;
    if (__builtin_strcmp(__old, "C"))
      {
	const size_t __len = __builtin_strlen(__old) + 1;
	__sav = new char[__len];
	__builtin_memcpy(__sav, __old, __len);
	setlocale(LC_CTYPE, "C");
      }
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    _M_toupper = *__ctype_toupper_loc();
    _M_tolower = *__ctype_tolower_loc();
    _M_table = __table ? __table : *__ctype_b_loc();
#else
    _M_toupper = __ctype_toupper;
    _M_tolower = __ctype_tolower;
    _M_table = __table ? __table : __ctype_b;
#endif
    if (__sav)
      {
	setlocale(LC_CTYPE, __sav);
	delete [] __sav;
      }
    __builtin_memset(_M_widen, 0, sizeof(_M_widen));
    __builtin_memset(_M_narrow, 0, sizeof(_M_narrow));
  }
#endif

#if _GLIBCXX_C_LOCALE_GNU
  ctype<char>::ctype(const mask* __table, bool __del, size_t __refs)
  : facet(__refs), _M_c_locale_ctype(_S_get_c_locale()),
  _M_del(__table != 0 && __del),
  _M_toupper(_M_c_locale_ctype->__ctype_toupper),
  _M_tolower(_M_c_locale_ctype->__ctype_tolower),
  _M_table(__table ? __table : _M_c_locale_ctype->__ctype_b),
  _M_widen_ok(0), _M_narrow_ok(0)
  {
    __builtin_memset(_M_widen, 0, sizeof(_M_widen));
    __builtin_memset(_M_narrow, 0, sizeof(_M_narrow));
  }
#else
  ctype<char>::ctype(const mask* __table, bool __del, size_t __refs)
  : facet(__refs), _M_c_locale_ctype(_S_get_c_locale()),
  _M_del(__table != 0 && __del), _M_widen_ok(0), _M_narrow_ok(0)
  {
    char* __old = setlocale(LC_CTYPE, NULL);
    char* __sav = NULL;
    if (__builtin_strcmp(__old, "C"))
      {
	const size_t __len = __builtin_strlen(__old) + 1;
	__sav = new char[__len];
	__builtin_memcpy(__sav, __old, __len);
	setlocale(LC_CTYPE, "C");
      }
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    _M_toupper = *__ctype_toupper_loc();
    _M_tolower = *__ctype_tolower_loc();
    _M_table = __table ? __table : *__ctype_b_loc();
#else
    _M_toupper = __ctype_toupper;
    _M_tolower = __ctype_tolower;
    _M_table = __table ? __table : __ctype_b;
#endif
    if (__sav)
      {
	setlocale(LC_CTYPE, __sav);
	delete [] __sav;
      }
    __builtin_memset(_M_widen, 0, sizeof(_M_widen));
    __builtin_memset(_M_narrow, 0, sizeof(_M_narrow));
  }
#endif

  char
  ctype<char>::do_toupper(char __c) const
  { return _M_toupper[static_cast<unsigned char>(__c)]; }

  const char*
  ctype<char>::do_toupper(char* __low, const char* __high) const
  {
    while (__low < __high)
      {
	*__low = _M_toupper[static_cast<unsigned char>(*__low)];
	++__low;
      }
    return __high;
  }

  char
  ctype<char>::do_tolower(char __c) const
  { return _M_tolower[static_cast<unsigned char>(__c)]; }

  const char*
  ctype<char>::do_tolower(char* __low, const char* __high) const
  {
    while (__low < __high)
      {
	*__low = _M_tolower[static_cast<unsigned char>(*__low)];
	++__low;
      }
    return __high;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
