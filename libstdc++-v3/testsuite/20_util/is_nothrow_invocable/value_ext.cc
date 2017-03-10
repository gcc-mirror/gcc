// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do compile { target c++11 } }

#include <type_traits>

template<typename... T>
  constexpr bool is_nt_invocable()
  { return std::__is_nothrow_invocable<T...>::value; }

  template<typename R, typename... T>
  constexpr bool is_nt_invocable_conv(std::true_type)
  {
    using result_type = typename std::__invoke_result<T...>::type;
    return std::is_void<R>::value || std::is_convertible<result_type, R>::value;
  }

template<typename R, typename... T>
  constexpr bool is_nt_invocable_conv(std::false_type) { return false; }

template<typename R, typename... T>
  constexpr bool is_nt_invocable_r()
  {
    return is_nt_invocable_conv<R, T...>(std::__is_nothrow_invocable<T...>{});
  }

#define IS_NT_INVOCABLE_DEFINED
#include "value.cc"
