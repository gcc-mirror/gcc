// Locale support -*- C++ -*-

// Copyright (C) 2000 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 22.1  Locales
//
  
// Information as gleaned from /usr/include/ctype.h on FreeBSD 3.4,
// 4.0 and all versions of the CVS managed file at:
// :pserver:anoncvs@anoncvs.freebsd.org:/home/ncvs/src/include/ctype.h
// which should cover most classic BSD configurations
  
  struct ctype_base
  {
    typedef unsigned long 	mask;
    // Non-standard typedefs.
    typedef const int* 		__to_type;

    enum
    {
#ifdef _CTYPE_S
      // FreeBSD 4.0 uses this style of define.
      space = _CTYPE_S,
      print = _CTYPE_R,
      cntrl = _CTYPE_C,
      upper = _CTYPE_U,
      lower = _CTYPE_L,
      alpha = _CTYPE_A,
      digit = _CTYPE_D,
      punct = _CTYPE_P,
      xdigit = _CTYPE_X,
      alnum = _CTYPE_A | _CTYPE_D,
      graph = _CTYPE_G
#else
      // Other BSD's, including Free BSD 3.4, uses this style of define.
      space = _S,
      print = _R,
      cntrl = _C,
      upper = _U,
      lower = _L,
      alpha = _A,
      digit = _D,
      punct = _P,
      xdigit = _X,
      alnum = _A | _D,
      graph = _G
#endif
    };
  };



