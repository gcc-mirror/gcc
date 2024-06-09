// -*- C++ -*- compatibility header.

// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

/** @file uchar.h
 *  This is a Standard C++ Library header.
 */

#include <cuchar>

#ifndef _GLIBCXX_UCHAR_H
#define _GLIBCXX_UCHAR_H 1

#ifdef _GLIBCXX_NAMESPACE_C

#if (_GLIBCXX_USE_CHAR8_T \
     && (_GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_FCHAR8_T \
	 || (__cplusplus >= 202002 \
	     && _GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_CXX20)))
using std::mbrtoc8;
using std::c8rtomb;
#endif // _GLIBCXX_USE_CHAR8_T

#if _GLIBCXX_USE_C11_UCHAR_CXX11
using std::mbrtoc16;
using std::c16rtomb;
using std::mbrtoc32;
using std::c32rtomb;
#endif // _GLIBCXX_USE_C11_UCHAR_CXX11

#endif // _GLIBCXX_NAMESPACE_C

#endif // _GLIBCXX_UCHAR_H
