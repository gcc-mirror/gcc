// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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
  struct ConvIsNothrow
  {
    using result_type = typename std::__invoke_result<T...>::type;
    static void test(std::true_type, R) noexcept;
    static void test(std::false_type, const result_type&);
    static constexpr bool value
      = noexcept(test(std::is_convertible<result_type, R>(),
		      std::declval<result_type>()));
  };

template<typename... T>
  struct ConvIsNothrow<void, T...> : std::true_type
  { };

template<typename R, typename... T>
  constexpr bool is_nt_invocable_conv(std::true_type)
  {
    return ConvIsNothrow<R, T...>::value;
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
