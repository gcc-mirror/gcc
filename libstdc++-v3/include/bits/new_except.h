// Exception classes for <new> -*- C++ -*-

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

/** @file bits/new_except.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{new}
 */

//
// ISO C++ 14882: 19.1  Exception classes
//

#ifndef _NEW_EXCEPT_H
#define _NEW_EXCEPT_H 1

#include <bits/c++config.h>
#include <bits/exception_defines.h>
#include <bits/exception.h>

extern "C++"
{

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @brief  Exception possibly thrown by @c new.
   *  @ingroup exceptions
   *
   *  @c bad_alloc (or classes derived from it) is used to report allocation
   *  errors from the throwing forms of @c new.  */
  class bad_alloc : public exception
  {
  public:
    _GLIBCXX26_CONSTEXPR bad_alloc() throw() { }

#if __cplusplus >= 201103L
    _GLIBCXX26_CONSTEXPR bad_alloc(const bad_alloc&) = default;
    _GLIBCXX26_CONSTEXPR bad_alloc& operator=(const bad_alloc&) = default;
#endif

#if __cplusplus >= 202400L
    [[__gnu__::__gnu_inline__]]
    constexpr inline virtual ~bad_alloc() noexcept {}

    [[__gnu__::__gnu_inline__]]
    constexpr inline virtual const char* what() const noexcept
    {
      return "std::bad_alloc";
    }
#else
    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_alloc() throw();

    // See comment in eh_exception.cc.
    virtual const char* what() const throw();
#endif
  };

#if __cplusplus >= 201103L
  class bad_array_new_length : public bad_alloc
  {
  public:
    _GLIBCXX26_CONSTEXPR bad_array_new_length() throw() { }

#if __cplusplus >= 202400L
    [[__gnu__::__gnu_inline__]]
    constexpr inline virtual ~bad_array_new_length() noexcept {}

    [[__gnu__::__gnu_inline__]]
    constexpr inline virtual const char* what() const noexcept
    {
      return "std::bad_array_new_length";
    }
#else
    // This declaration is not useless:
    // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
    virtual ~bad_array_new_length() throw();

    // See comment in eh_exception.cc.
    virtual const char* what() const throw();
#endif
  };
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

}

#endif
