// Locale support -*- C++ -*-

// Copyright (C) 2000-2026 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.1  Locales
//

// Information as gleaned from /usr/include/ctype.h on OpenBSD.

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// @brief  Base class for ctype.
  struct ctype_base
  {
    // Non-standard typedefs.
    typedef const short*	__to_type;

    // NB: Offsets into ctype<char>::_M_table force a particular size
    // on the mask type. Because of this, we don't use an enum.
    typedef char		mask;

#ifdef _CTYPE_S
    // OpenBSD 7.5 uses this style of define.
    static const mask upper	= static_cast<mask>(_CTYPE_U);
    static const mask lower	= static_cast<mask>(_CTYPE_L);
    static const mask alpha	= static_cast<mask>(_CTYPE_U | _CTYPE_L);
    static const mask digit	= static_cast<mask>(_CTYPE_N);
    static const mask xdigit	= static_cast<mask>(_CTYPE_N | _CTYPE_X);
    static const mask space	= static_cast<mask>(_CTYPE_S);
    static const mask print	= static_cast<mask>(_CTYPE_P | _CTYPE_U | _CTYPE_L | _CTYPE_N | _CTYPE_B);
    static const mask graph	= static_cast<mask>(_CTYPE_P | _CTYPE_U | _CTYPE_L | _CTYPE_N);
    static const mask cntrl	= static_cast<mask>(_CTYPE_C);
    static const mask punct	= static_cast<mask>(_CTYPE_P);
    static const mask alnum	= static_cast<mask>(_CTYPE_U | _CTYPE_L | _CTYPE_N);
#else
    // Older versions use this style.
    static const mask upper	= static_cast<mask>(_U);
    static const mask lower	= static_cast<mask>(_L);
    static const mask alpha	= static_cast<mask>(_U | _L);
    static const mask digit	= static_cast<mask>(_N);
    static const mask xdigit	= static_cast<mask>(_N | _X);
    static const mask space	= static_cast<mask>(_S);
    static const mask print	= static_cast<mask>(_P | _U | _L | _N | _B);
    static const mask graph	= static_cast<mask>(_P | _U | _L | _N);
    static const mask cntrl	= static_cast<mask>(_C);
    static const mask punct	= static_cast<mask>(_P);
    static const mask alnum	= static_cast<mask>(_U | _L | _N);
#endif
#if __cplusplus >= 201103L
    static const mask blank	= space;
#endif
  };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
