// Function-Based Exception Support -*- C++ -*-

// Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

/** @file bits/new_throw.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{new}
 */

//
// ISO C++ 14882: 19.1  Exception classes
//

#ifndef _NEW_THROW_H
#define _NEW_THROW_H 1

#include <bits/c++config.h>
#include <bits/exception_defines.h>
#if (_GLIBCXX_HOSTED && __cpp_exceptions && __cplusplus > 202302L \
     && __cpp_constexpr_exceptions >= 202411L)
#include <bits/new_except.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#if _GLIBCXX_HOSTED
#if (__cpp_exceptions && __cplusplus > 202302L \
     && __cpp_constexpr_exceptions >= 202411L)
  // Helper for exception objects in <new>
  [[noreturn, __gnu__::__always_inline__]] constexpr void
  __throw_bad_alloc(void)
  {
    throw bad_alloc();
  }

  [[noreturn, __gnu__::__always_inline__]] constexpr void
  __throw_bad_array_new_length(void)
  {
    throw bad_array_new_length();
  }
#else
  // Helper for exception objects in <new>
  void
  __throw_bad_alloc(void) __attribute__((__noreturn__));

  void
  __throw_bad_array_new_length(void) __attribute__((__noreturn__));
#endif
#endif // HOSTED

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif
