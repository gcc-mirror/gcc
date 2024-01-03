// std::ctype implementation details, generic version -*- C++ -*-

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

#include <locale>
#include <cstdlib>
#include <cstring>
#include <cstdio>

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
	__ret = wctype("space");
	break;
      case print:
	__ret = wctype("print");
	break;
      case cntrl:
	__ret = wctype("cntrl");
	break;
      case upper:
	__ret = wctype("upper");
	break;
      case lower:
	__ret = wctype("lower");
	break;
      case alpha:
	__ret = wctype("alpha");
	break;
      case digit:
	__ret = wctype("digit");
	break;
      case punct:
	__ret = wctype("punct");
	break;
      case xdigit:
	__ret = wctype("xdigit");
	break;
      case alnum:
	__ret = wctype("alnum");
	break;
      case graph:
	__ret = wctype("graph");
	break;
      default:
	// For some targets ctype_base::blank == ctype_base::space so check
	// here to avoid a duplicate case error.
	if (__m == blank)
	  __ret = wctype("blank");
	else
	  __ret = __wmask_type();
      }
    return __ret;
  };

  wchar_t
  ctype<wchar_t>::do_toupper(wchar_t __c) const
  { return towupper(__c); }

  const wchar_t*
  ctype<wchar_t>::do_toupper(wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi)
      {
        *__lo = towupper(*__lo);
        ++__lo;
      }
    return __hi;
  }

  wchar_t
  ctype<wchar_t>::do_tolower(wchar_t __c) const
  { return towlower(__c); }

  const wchar_t*
  ctype<wchar_t>::do_tolower(wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi)
      {
        *__lo = towlower(*__lo);
        ++__lo;
      }
    return __hi;
  }

  bool
  ctype<wchar_t>::
  do_is(mask __m, char_type __c) const
  {
    bool __ret = false;
    // Generically, 15 (instead of 11) since we don't know the numerical
    // encoding of the various categories in /usr/include/ctype.h.
    const size_t __bitmasksize = 15;
    for (size_t __bitcur = 0; __bitcur <= __bitmasksize; ++__bitcur)
      if (__m & _M_bit[__bitcur]
	  && iswctype(__c, _M_wmask[__bitcur]))
	{
	  __ret = true;
	  break;
	}
    return __ret;
  }

  const wchar_t*
  ctype<wchar_t>::
  do_is(const wchar_t* __lo, const wchar_t* __hi, mask* __vec) const
  {
    for (;__lo < __hi; ++__vec, ++__lo)
      {
	// Generically, 15 (instead of 11) since we don't know the numerical
	// encoding of the various categories in /usr/include/ctype.h.
	const size_t __bitmasksize = 15;
	mask __m = 0;
	for (size_t __bitcur = 0; __bitcur <= __bitmasksize; ++__bitcur)
	  if (iswctype(*__lo, _M_wmask[__bitcur]))
	    __m |= _M_bit[__bitcur];
	*__vec = __m;
      }
    return __hi;
  }

  const wchar_t*
  ctype<wchar_t>::
  do_scan_is(mask __m, const wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi && !this->do_is(__m, *__lo))
      ++__lo;
    return __lo;
  }

  const wchar_t*
  ctype<wchar_t>::
  do_scan_not(mask __m, const char_type* __lo, const char_type* __hi) const
  {
    while (__lo < __hi && this->do_is(__m, *__lo) != 0)
      ++__lo;
    return __lo;
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
    const int __c = wctob(__wc);
    return (__c == EOF ? __dfault : static_cast<char>(__c));
  }

  const wchar_t*
  ctype<wchar_t>::
  do_narrow(const wchar_t* __lo, const wchar_t* __hi, char __dfault,
	    char* __dest) const
  {
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
    return __hi;
  }

  void
  ctype<wchar_t>::_M_initialize_ctype() throw()
  {
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
    for (size_t __i = 0;
	 __i < sizeof(_M_widen) / sizeof(wint_t); ++__i)
      _M_widen[__i] = btowc(__i);

    for (size_t __i = 0; __i <= 15; ++__i)
      {
	_M_bit[__i] = static_cast<mask>(1 << __i);
	_M_wmask[__i] = _M_convert_to_wmask(_M_bit[__i]);
      }
  }
#endif //  _GLIBCXX_USE_WCHAR_T

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
