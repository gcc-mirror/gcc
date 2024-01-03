// -*- C++ -*- compatibility header.

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

/** @file inttypes.h
 *  This is a Standard C++ Library header.
 */

#ifndef _GLIBCXX_INTTYPES_H
#define _GLIBCXX_INTTYPES_H 1

#pragma GCC system_header

#include <bits/c++config.h>

#if __cplusplus >= 201103L

// For 8.11.1/1 (see C99, Note 184)
# if _GLIBCXX_HAVE_INTTYPES_H
#  ifndef __STDC_FORMAT_MACROS
#   define _UNDEF__STDC_FORMAT_MACROS
#   define __STDC_FORMAT_MACROS
#  endif
#  include_next <inttypes.h>
#  ifdef _UNDEF__STDC_FORMAT_MACROS
#   undef __STDC_FORMAT_MACROS
#   undef _UNDEF__STDC_FORMAT_MACROS
#  endif
# endif

#ifdef _GLIBCXX_USE_C99_INTTYPES

namespace std
{
  // types
  using ::imaxdiv_t;

  // functions
  using ::imaxabs;

  // May collide with _Longlong abs(_Longlong), and is not described
  // anywhere outside the synopsis.  Likely, a defect.
  //
  // intmax_t abs(intmax_t)

  using ::imaxdiv;

  // Likewise, with lldiv_t div(_Longlong, _Longlong).
  //
  // imaxdiv_t div(intmax_t, intmax_t)

  using ::strtoimax;
  using ::strtoumax;

#if defined(_GLIBCXX_USE_WCHAR_T) && _GLIBCXX_USE_C99_INTTYPES_WCHAR_T
  using ::wcstoimax;
  using ::wcstoumax;
#endif
} // namespace

#endif _GLIBCXX_USE_C99_INTTYPES

#else

# if _GLIBCXX_HAVE_INTTYPES_H
#  include_next <inttypes.h>
# endif

#endif // C++11

#endif // _GLIBCXX_INTTYPES_H
