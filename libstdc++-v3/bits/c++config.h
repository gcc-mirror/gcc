// Predefined symbols and macros -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

#ifndef _CPP_CPPCONFIG
#define _CPP_CPPCONFIG 1

// The current version of the C++ library in compressed ISO date format.
#define __GLIBCPP__ 20000324

// This flag controls the error handling in string, and perhaps other
// bits as time goes on: check out bits/basic_string.h for more
// info. It also helps alleviate the circular dependency between
// string and exception.
# define _GLIBCPP_USE_EXCEPTIONS 1 

// This is necessary until Egcs supports separate template
// compilation.  
#define _GLIBCPP_NO_TEMPLATE_EXPORT 1

// This is a hack around not having either pre-compiled headers or
// export compilation. If defined, the io, string, and valarray
// headers will include all the necessary bits. If not defined, the
// implementation optimizes the headers for the most commonly-used
// types. For the io library, this means that larger, out-of-line
// member functions are only declared, and definitions are not parsed
// by the compiler, but instead instantiated into the library binary.
//#define _GLIBCPP_FULLY_COMPLIANT_HEADERS 1

// To enable older, ARM-style iostreams and other anachronisms use this.
//#define _GLIBCPP_DEPRICATED 1

// Use corrected code from the committee library group's issues list.
# define _GLIBCPP_RESOLVE_LIB_DEFECTS 1







