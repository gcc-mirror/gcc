// Locale support -*- C++ -*-

// Copyright (C) 2000-2014 Free Software Foundation, Inc.
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
  
// Information as gleaned from /usr/include/ctype.h on FreeBSD 3.4,
// 4.0 and all versions of the CVS managed file at:
// :pserver:anoncvs@anoncvs.freebsd.org:/home/ncvs/src/include/ctype.h
  
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// @brief  Base class for ctype.
  struct ctype_base
  {
    // Non-standard typedefs.
    typedef const int* 		__to_type;

    typedef unsigned long 	mask;
#ifdef _CTYPE_S
    // FreeBSD 4.0 uses this style of define.
    static const mask upper    	= _CTYPE_U;
    static const mask lower 	= _CTYPE_L;
    static const mask alpha 	= _CTYPE_A;
    static const mask digit 	= _CTYPE_D;
    static const mask xdigit 	= _CTYPE_X;
    static const mask space 	= _CTYPE_S;
    static const mask print 	= _CTYPE_R;
    static const mask graph 	= _CTYPE_A | _CTYPE_D | _CTYPE_P;
    static const mask cntrl 	= _CTYPE_C;
    static const mask punct 	= _CTYPE_P;
    static const mask alnum 	= _CTYPE_A | _CTYPE_D;
#else
    // Older versions, including Free BSD 3.4, use this style of define.
    static const mask upper    	= _U;
    static const mask lower 	= _L;
    static const mask alpha 	= _A;
    static const mask digit 	= _D;
    static const mask xdigit 	= _X;
    static const mask space 	= _S;
    static const mask print 	= _R;
    static const mask graph 	= _A | _D | _P;
    static const mask cntrl 	= _C;
    static const mask punct 	= _P;
    static const mask alnum 	= _A | _D;
#endif
  };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
