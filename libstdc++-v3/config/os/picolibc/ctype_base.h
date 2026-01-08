// Locale support for picolibc -*- C++ -*-

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

// Information as gleaned from /usr/include/ctype.h

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// @brief  Base class for ctype.
  struct ctype_base
  {
    // Non-standard typedefs.
    typedef const int* 		__to_type;

    // NB: Offsets into ctype<char>::_M_table force a particular size
    // on the mask type. Because of this, we don't use an enum.
    typedef short 		mask;
    static const mask upper    	= mask (__CTYPE_UPPER);
    static const mask lower 	= mask (__CTYPE_LOWER);
    static const mask alpha 	= mask (__CTYPE_UPPER | __CTYPE_LOWER);
    static const mask digit 	= mask (__CTYPE_DIGIT);
    static const mask xdigit 	= mask (__CTYPE_HEX | __CTYPE_DIGIT);
    static const mask space 	= mask (__CTYPE_SPACE);
    static const mask print 	= mask (__CTYPE_PUNCT | __CTYPE_UPPER | __CTYPE_LOWER | __CTYPE_DIGIT | __CTYPE_BLANK);
    static const mask graph 	= mask (__CTYPE_PUNCT | __CTYPE_UPPER | __CTYPE_LOWER | __CTYPE_DIGIT);
    static const mask cntrl 	= mask (__CTYPE_CNTRL);
    static const mask punct 	= mask (__CTYPE_PUNCT);
    static const mask alnum 	= mask (__CTYPE_UPPER | __CTYPE_LOWER | __CTYPE_DIGIT);
#if __cplusplus >= 201103L
    static const mask blank 	= mask (__CTYPE_BLANK | __CTYPE_TAB);
#endif
  };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
