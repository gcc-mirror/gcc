// Methods and support infrastructure for exceptions -*- C++ -*-

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
// ISO C++ 14882: 15 Exception handling
//

// This file declares functions whose only purpose is to throw an
// exception. They help break a circularity between <string> and
// <stdexcept>. See src/stdexcept.cc, where these functions are
// defined.

// XXX: These functions serve a similar purpose to those in
// stl/bits/stl_range_errors.h . Eventually the two approaches should
// be merged. 

#ifndef _CPP_EXCEPTION_SUPPORT_H
#define _CPP_EXCEPTION_SUPPORT_H	1

namespace std {

#if _GLIBCPP_USE_EXCEPTIONS
  // Internal functions for string implementation.
  extern void __out_of_range(const char *__str);
  extern void __length_error(const char *__str);
  
# define __OUTOFRANGE(__cond) \
  do { if (__cond) __out_of_range(#__cond); } while (0)
# define __LENGTHERROR(__cond) \
  do { if (__cond) __length_error(#__cond); } while (0)
#else
# include <bits/std_cassert.h>
# define __OUTOFRANGE(__cond) assert(!(__cond))
# define __LENGTHERROR(__cond) assert(!(__cond))
#endif

} // namespace std

#endif	/* _CPP_EXCEPTION_SUPPORT_H */














