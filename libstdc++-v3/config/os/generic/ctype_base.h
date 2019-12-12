// Locale support -*- C++ -*-

// Copyright (C) 1997-2019 Free Software Foundation, Inc.
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

// Default information, may not be appropriate for specific host.

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
    typedef unsigned int 	mask;
    static const mask upper    	= 1 << 0;
    static const mask lower 	= 1 << 1;
    static const mask alpha 	= 1 << 2;
    static const mask digit 	= 1 << 3;
    static const mask xdigit 	= 1 << 4;
    static const mask space 	= 1 << 5;
    static const mask print 	= 1 << 6;
    static const mask graph 	= (1 << 2) | (1 << 3) | (1 << 9); // alnum|punct
    static const mask cntrl 	= 1 << 8;
    static const mask punct 	= 1 << 9;
    static const mask alnum 	= (1 << 2) | (1 << 3);  // alpha|digit
    static const mask blank	= 1 << 10;
  };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
