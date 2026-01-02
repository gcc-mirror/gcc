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

/** @file bits/stdexcept_throwfwd.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{stdexcept}
 */

//
// ISO C++ 14882: 19.1  Exception classes
//

#ifndef _STDEXCEPT_THROWFWD_H
#define _STDEXCEPT_THROWFWD_H 1

#include <bits/c++config.h>
#include <bits/exception_defines.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#if _GLIBCXX_HOSTED
#if (__cpp_exceptions && __cplusplus > 202302L \
     && __cpp_constexpr_exceptions >= 202411L)
  // Helper for exception objects in <stdexcept>
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_logic_error(const char*);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_domain_error(const char*);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_invalid_argument(const char*);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_length_error(const char*);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_out_of_range(const char*);
  template <typename... _Args>
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_out_of_range_fmt(const char* __s, _Args... __args);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_runtime_error(const char*);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_overflow_error(const char*);
  [[noreturn, __gnu__::__always_inline__, __gnu__::__cold__]] constexpr void
  __throw_underflow_error(const char*);
#else
  // Helpers for exception objects in <stdexcept>
  void
  __throw_logic_error(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_domain_error(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_invalid_argument(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_length_error(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_out_of_range(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_out_of_range_fmt(const char*, ...)
    __attribute__((__noreturn__,__cold__,__format__(__gnu_printf__, 1, 2)));

  void
  __throw_runtime_error(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_overflow_error(const char*) __attribute__((__noreturn__,__cold__));

  void
  __throw_underflow_error(const char*) __attribute__((__noreturn__,__cold__));
#endif

#else // ! HOSTED

  __attribute__((__noreturn__)) inline void
  __throw_invalid_argument(const char*)
  { std::__terminate(); }

  __attribute__((__noreturn__)) inline void
  __throw_out_of_range(const char*)
  { std::__terminate(); }

  __attribute__((__noreturn__)) inline void
  __throw_out_of_range_fmt(const char*, ...)
  { std::__terminate(); }

  __attribute__((__noreturn__)) inline void
  __throw_runtime_error(const char*)
  { std::__terminate(); }

  __attribute__((__noreturn__)) inline void
  __throw_overflow_error(const char*)
  { std::__terminate(); }

#endif // HOSTED

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif
