// std::ctype implementation details, DragonFly version -*- C++ -*-

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.2.1.1.2  ctype virtual functions.
//

// Written by Benjamin Kosnik <bkoz@redhat.com>
// Modified for DragonFly by John Marino <gnugcc@marino.st>

#include <locale>
#include <cstring>
#include <cstdio>

#ifndef _ISbit
#define _ISbit(bit) ((bit) < 8 ? ((1 << (bit)) << 8) : ((1 << (bit)) >> 8))
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // NB: The other ctype<char> specializations are in src/locale.cc and
  // various /config/os/* files.
  ctype_byname<char>::ctype_byname(const char* __s, size_t __refs)
  : ctype<char>(0, false, __refs)
  {
    if (std::strcmp(__s, "C") != 0 && std::strcmp(__s, "POSIX") != 0)
      {
	this->_S_destroy_c_locale(this->_M_c_locale_ctype);
	this->_S_create_c_locale(this->_M_c_locale_ctype, __s);
      }
  }

  ctype_byname<char>::~ctype_byname()
  { }

#ifdef _GLIBCXX_USE_WCHAR_T
  ctype<wchar_t>::__wmask_type
  ctype<wchar_t>::_M_convert_to_wmask(const mask __m) const throw()
  {
    __wmask_type __ret;
    switch (__m)
      {
      case space:
	__ret = wctype_l("space", (locale_t)_M_c_locale_ctype);
	break;
      case print:
	__ret = wctype_l("print", (locale_t)_M_c_locale_ctype);
	break;
      case cntrl:
	__ret = wctype_l("cntrl", (locale_t)_M_c_locale_ctype);
	break;
      case upper:
	__ret = wctype_l("upper", (locale_t)_M_c_locale_ctype);
	break;
      case lower:
	__ret = wctype_l("lower", (locale_t)_M_c_locale_ctype);
	break;
      case alpha:
	__ret = wctype_l("alpha", (locale_t)_M_c_locale_ctype);
	break;
      case digit:
	__ret = wctype_l("digit", (locale_t)_M_c_locale_ctype);
	break;
      case punct:
	__ret = wctype_l("punct", (locale_t)_M_c_locale_ctype);
	break;
      case xdigit:
	__ret = wctype_l("xdigit", (locale_t)_M_c_locale_ctype);
	break;
      case alnum:
	__ret = wctype_l("alnum", (locale_t)_M_c_locale_ctype);
	break;
      case graph:
	__ret = wctype_l("graph", (locale_t)_M_c_locale_ctype);
	break;
      case blank:
	__ret = wctype_l("blank", (locale_t)_M_c_locale_ctype);
	break;
      default:
	__ret = __wmask_type();
      }
    return __ret;
  }

  wchar_t
  ctype<wchar_t>::do_toupper(wchar_t __c) const
  { return towupper_l(__c, (locale_t)_M_c_locale_ctype); }

  const wchar_t*
  ctype<wchar_t>::do_toupper(wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi)
      {
        *__lo = towupper_l(*__lo, (locale_t)_M_c_locale_ctype);
        ++__lo;
      }
    return __hi;
  }

  wchar_t
  ctype<wchar_t>::do_tolower(wchar_t __c) const
  { return towlower_l(__c, (locale_t)_M_c_locale_ctype); }

  const wchar_t*
  ctype<wchar_t>::do_tolower(wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi)
      {
        *__lo = towlower_l(*__lo, (locale_t)_M_c_locale_ctype);
        ++__lo;
      }
    return __hi;
  }

  wchar_t
  ctype<wchar_t>::
  do_widen(char __c) const
  { return _M_widen[static_cast<unsigned char>(__c)]; }

  const char*
  ctype<wchar_t>::
  do_widen(const char* __lo, const char* __hi, wchar_t* __dest) const
  {
    while (__lo < __hi)
      {
	*__dest = _M_widen[static_cast<unsigned char>(*__lo)];
	++__lo;
	++__dest;
      }
    return __hi;
  }

  char
  ctype<wchar_t>::
  do_narrow(wchar_t __wc, char __dfault) const
  {
    if (__wc >= 0 && __wc < 128 && _M_narrow_ok)
      return _M_narrow[__wc];
    __c_locale __old = (__c_locale)uselocale((locale_t)_M_c_locale_ctype);
    const int __c = wctob(__wc);
    uselocale((locale_t)__old);
    return (__c == EOF ? __dfault : static_cast<char>(__c));
  }

  const wchar_t*
  ctype<wchar_t>::
  do_narrow(const wchar_t* __lo, const wchar_t* __hi, char __dfault,
	    char* __dest) const
  {
    __c_locale __old = (__c_locale)uselocale((locale_t)_M_c_locale_ctype);
    if (_M_narrow_ok)
      while (__lo < __hi)
	{
	  if (*__lo >= 0 && *__lo < 128)
	    *__dest = _M_narrow[*__lo];
	  else
	    {
	      const int __c = wctob(*__lo);
	      *__dest = (__c == EOF ? __dfault : static_cast<char>(__c));
	    }
	  ++__lo;
	  ++__dest;
	}
    else
      while (__lo < __hi)
	{
	  const int __c = wctob(*__lo);
	  *__dest = (__c == EOF ? __dfault : static_cast<char>(__c));
	  ++__lo;
	  ++__dest;
	}
    uselocale((locale_t)__old);
    return __hi;
  }

  void
  ctype<wchar_t>::_M_initialize_ctype() throw()
  {
    __c_locale __old = (__c_locale)uselocale((locale_t)_M_c_locale_ctype);
    wint_t __i;
    for (__i = 0; __i < 128; ++__i)
      {
	const int __c = wctob(__i);
	if (__c == EOF)
	  break;
	else
	  _M_narrow[__i] = static_cast<char>(__c);
      }
    if (__i == 128)
      _M_narrow_ok = true;
    else
      _M_narrow_ok = false;
    for (size_t __j = 0;
	 __j < sizeof(_M_widen) / sizeof(wint_t); ++__j)
      _M_widen[__j] = btowc(__j);

    for (size_t __k = 0; __k <= 11; ++__k)
      {
	_M_bit[__k] = static_cast<mask>(_ISbit(__k));
	_M_wmask[__k] = _M_convert_to_wmask(_M_bit[__k]);
      }
    uselocale((locale_t)__old);
  }
#endif //  _GLIBCXX_USE_WCHAR_T

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
