// Underlying io library details -*- C++ -*-

// Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

/** @file bits/c++io.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{ios}
 */

// c_io_stdio.h - Defines for using "C" stdio.h

#ifndef _GLIBCXX_CXX_IO_H
#define _GLIBCXX_CXX_IO_H 1

#include <cstdio>
#include <bits/gthr.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#ifdef __GTHREAD_LEGACY_MUTEX_T
  // The layout of __gthread_mutex_t changed in GCC 13, but libstdc++ doesn't
  // actually use the basic_filebuf::_M_lock member, so define it consistently
  // with the old __gthread_mutex_t to avoid an unnecessary layout change:
  typedef __GTHREAD_LEGACY_MUTEX_T __c_lock;
#else
  typedef __gthread_mutex_t __c_lock;
#endif

  // for basic_file.h
  typedef FILE __c_file;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif
