// Locale support -*- C++ -*-

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

  // The classic table used in libstdc++ is *not* the C _ctype table
  // used by mscvrt, but is based on the ctype masks defined for libstdc++
  // in ctype_base.h.

  const ctype_base::mask*
  ctype<char>::classic_table() throw()
  {
    static const ctype_base::mask _S_classic_table[256] =
    {
      cntrl /* null */,
      cntrl /* ^A */,
      cntrl /* ^B */,
      cntrl /* ^C */,
      cntrl /* ^D */,
      cntrl /* ^E */,
      cntrl /* ^F */,
      cntrl /* ^G */,
      cntrl /* ^H */,
      ctype_base::mask(space | cntrl | blank) /* tab */,
      ctype_base::mask(space | cntrl) /* LF */,
      ctype_base::mask(space | cntrl) /* ^K */,
      ctype_base::mask(space | cntrl) /* FF */,
      ctype_base::mask(space | cntrl) /* ^M */,
      cntrl /* ^N */,
      cntrl /* ^O */,
      cntrl /* ^P */,
      cntrl /* ^Q */,
      cntrl /* ^R */,
      cntrl /* ^S */,
      cntrl /* ^T */,
      cntrl /* ^U */,
      cntrl /* ^V */,
      cntrl /* ^W */,
      cntrl /* ^X */,
      cntrl /* ^Y */,
      cntrl /* ^Z */,
      cntrl /* esc */,
      cntrl /* ^\ */,
      cntrl /* ^] */,
      cntrl /* ^^ */,
      cntrl /* ^_ */,
      ctype_base::mask(space | print | blank) /*   */,
      ctype_base::mask(punct | print) /* ! */,
      ctype_base::mask(punct | print) /* " */,
      ctype_base::mask(punct | print) /* # */,
      ctype_base::mask(punct | print) /* $ */,
      ctype_base::mask(punct | print) /* % */,
      ctype_base::mask(punct | print) /* & */,
      ctype_base::mask(punct | print) /* ' */,
      ctype_base::mask(punct | print) /* ( */,
      ctype_base::mask(punct | print) /* ) */,
      ctype_base::mask(punct | print) /* * */,
      ctype_base::mask(punct | print) /* + */,
      ctype_base::mask(punct | print) /* , */,
      ctype_base::mask(punct | print) /* - */,
      ctype_base::mask(punct | print) /* . */,
      ctype_base::mask(punct | print) /* / */,
      ctype_base::mask(digit | xdigit | print) /* 0 */,
      ctype_base::mask(digit | xdigit | print) /* 1 */,
      ctype_base::mask(digit | xdigit | print) /* 2 */,
      ctype_base::mask(digit | xdigit | print) /* 3 */,
      ctype_base::mask(digit | xdigit | print) /* 4 */,
      ctype_base::mask(digit | xdigit | print) /* 5 */,
      ctype_base::mask(digit | xdigit | print) /* 6 */,
      ctype_base::mask(digit | xdigit | print) /* 7 */,
      ctype_base::mask(digit | xdigit | print) /* 8 */,
      ctype_base::mask(digit | xdigit | print) /* 9 */,
      ctype_base::mask(punct | print) /* : */,
      ctype_base::mask(punct | print) /* ; */,
      ctype_base::mask(punct | print) /* < */,
      ctype_base::mask(punct | print) /* = */,
      ctype_base::mask(punct | print) /* > */,
      ctype_base::mask(punct | print) /* ? */,
      ctype_base::mask(punct | print) /* ! */,
      ctype_base::mask(alpha | upper | xdigit | print) /* A */,
      ctype_base::mask(alpha | upper | xdigit | print) /* B */,
      ctype_base::mask(alpha | upper | xdigit | print) /* C */,
      ctype_base::mask(alpha | upper | xdigit | print) /* D */,
      ctype_base::mask(alpha | upper | xdigit | print) /* E */,
      ctype_base::mask(alpha | upper | xdigit | print) /* F */,
      ctype_base::mask(alpha | upper | print) /* G */,
      ctype_base::mask(alpha | upper | print) /* H */,
      ctype_base::mask(alpha | upper | print) /* I */,
      ctype_base::mask(alpha | upper | print) /* J */,
      ctype_base::mask(alpha | upper | print) /* K */,
      ctype_base::mask(alpha | upper | print) /* L */,
      ctype_base::mask(alpha | upper | print) /* M */,
      ctype_base::mask(alpha | upper | print) /* N */,
      ctype_base::mask(alpha | upper | print) /* O */,
      ctype_base::mask(alpha | upper | print) /* P */,
      ctype_base::mask(alpha | upper | print) /* Q */,
      ctype_base::mask(alpha | upper | print) /* R */,
      ctype_base::mask(alpha | upper | print) /* S */,
      ctype_base::mask(alpha | upper | print) /* T */,
      ctype_base::mask(alpha | upper | print) /* U */,
      ctype_base::mask(alpha | upper | print) /* V */,
      ctype_base::mask(alpha | upper | print) /* W */,
      ctype_base::mask(alpha | upper | print) /* X */,
      ctype_base::mask(alpha | upper | print) /* Y */,
      ctype_base::mask(alpha | upper | print) /* Z */,
      ctype_base::mask(punct | print) /* [ */,
      ctype_base::mask(punct | print) /* \ */,
      ctype_base::mask(punct | print) /* ] */,
      ctype_base::mask(punct | print) /* ^ */,
      ctype_base::mask(punct | print) /* _ */,
      ctype_base::mask(punct | print) /* ` */,
      ctype_base::mask(alpha | lower | xdigit | print) /* a */,
      ctype_base::mask(alpha | lower | xdigit | print) /* b */,
      ctype_base::mask(alpha | lower | xdigit | print) /* c */,
      ctype_base::mask(alpha | lower | xdigit | print) /* d */,
      ctype_base::mask(alpha | lower | xdigit | print) /* e */,
      ctype_base::mask(alpha | lower | xdigit | print) /* f */,
      ctype_base::mask(alpha | lower | print) /* g */,
      ctype_base::mask(alpha | lower | print) /* h */,
      ctype_base::mask(alpha | lower | print) /* i */,
      ctype_base::mask(alpha | lower | print) /* j */,
      ctype_base::mask(alpha | lower | print) /* k */,
      ctype_base::mask(alpha | lower | print) /* l */,
      ctype_base::mask(alpha | lower | print) /* m */,
      ctype_base::mask(alpha | lower | print) /* n */,
      ctype_base::mask(alpha | lower | print) /* o */,
      ctype_base::mask(alpha | lower | print) /* p */,
      ctype_base::mask(alpha | lower | print) /* q */,
      ctype_base::mask(alpha | lower | print) /* r */,
      ctype_base::mask(alpha | lower | print) /* s */,
      ctype_base::mask(alpha | lower | print) /* t */,
      ctype_base::mask(alpha | lower | print) /* u */,
      ctype_base::mask(alpha | lower | print) /* v */,
      ctype_base::mask(alpha | lower | print) /* w */,
      ctype_base::mask(alpha | lower | print) /* x */,
      ctype_base::mask(alpha | lower | print) /* y */,
      ctype_base::mask(alpha | lower | print) /* x */,
      ctype_base::mask(punct | print) /* { */,
      ctype_base::mask(punct | print) /* | */,
      ctype_base::mask(punct | print) /* } */,
      ctype_base::mask(punct | print) /* ~ */,
      cntrl /* del (0x7f)*/,
      /* The next 128 entries are all 0.   */
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };
    return _S_classic_table;
  }

  ctype<char>::ctype(__c_locale, const mask* __table, bool __del,
		     size_t __refs)
  : facet(__refs), _M_del(__table != 0 && __del),
  _M_toupper(NULL), _M_tolower(NULL),
  _M_table(__table ? __table : classic_table())
  {
    memset(_M_widen, 0, sizeof(_M_widen));
    _M_widen_ok = 0;
    memset(_M_narrow, 0, sizeof(_M_narrow));
    _M_narrow_ok = 0;
  }

  ctype<char>::ctype(const mask* __table, bool __del, size_t __refs)
  : facet(__refs), _M_del(__table != 0 && __del),
  _M_toupper(NULL), _M_tolower(NULL),
  _M_table(__table ? __table : classic_table())
  {
    memset(_M_widen, 0, sizeof(_M_widen));
    _M_widen_ok = 0;
    memset(_M_narrow, 0, sizeof(_M_narrow));
    _M_narrow_ok = 0;
  }

  char
  ctype<char>::do_toupper(char __c) const
  { return (this->is(ctype_base::lower, __c) ? (__c - 'a' + 'A') : __c); }

  const char*
  ctype<char>::do_toupper(char* __low, const char* __high) const
  {
    while (__low < __high)
      {
	*__low = this->do_toupper(*__low);
	++__low;
      }
    return __high;
  }

  char
  ctype<char>::do_tolower(char __c) const
  { return (this->is(ctype_base::upper, __c) ? (__c - 'A' + 'a') : __c); }

  const char*
  ctype<char>::do_tolower(char* __low, const char* __high) const
  {
    while (__low < __high)
      {
	*__low = this->do_tolower(*__low);
	++__low;
      }
    return __high;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
