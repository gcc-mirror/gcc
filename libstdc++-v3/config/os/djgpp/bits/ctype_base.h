// Locale support -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
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
  
  struct ctype_base
  {
    typedef unsigned short 	mask;
    
    // Non-standard typedefs.
    typedef unsigned char	__to_type;

    enum
    {
      space = __dj_ISSPACE,	// Whitespace
      print = __dj_ISPRINT,	// Printing
      cntrl = __dj_ISCNTRL,	// Control character
      upper = __dj_ISUPPER,	// UPPERCASE
      lower = __dj_ISLOWER,	// lowercase
      alpha = __dj_ISALPHA,	// Alphabetic
      digit = __dj_ISDIGIT,	// Numeric
      punct = __dj_ISPUNCT,     // Punctuation
      xdigit = __dj_ISXDIGIT,   // Hexadecimal numeric
      alnum = __dj_ISAL,        // Alphanumeric
      graph = __dj_ISGRAPH	// Graphical
    };
  };



