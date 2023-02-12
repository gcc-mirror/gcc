// Locale support -*- C++ -*-

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// @brief  Base class for ctype.
  struct ctype_base
  {
    typedef unsigned short 	mask;

    // Non-standard typedefs.
    typedef unsigned char *     __to_type;

    // NB: Offsets into ctype<char>::_M_table force a particular size
    // on the mask type. Because of this, we don't use an enum.
    static const mask space = __dj_ISSPACE;	// Whitespace
    static const mask print = __dj_ISPRINT;	// Printing
    static const mask cntrl = __dj_ISCNTRL;	// Control character
    static const mask upper = __dj_ISUPPER;	// UPPERCASE
    static const mask lower = __dj_ISLOWER;	// lowercase
    static const mask alpha = __dj_ISALPHA;	// Alphabetic
    static const mask digit = __dj_ISDIGIT;	// Numeric
    static const mask punct = __dj_ISPUNCT;     // Punctuation
    static const mask xdigit = __dj_ISXDIGIT;   // Hexadecimal numeric
    static const mask alnum = __dj_ISALPHA | __dj_ISDIGIT;  // Alphanumeric
    static const mask graph = __dj_ISALPHA | __dj_ISDIGIT | __dj_ISPUNCT;  // Graphical
#if __cplusplus >= 201103L
    static const mask blank	= __dj_ISBLANK;
#endif
  };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
