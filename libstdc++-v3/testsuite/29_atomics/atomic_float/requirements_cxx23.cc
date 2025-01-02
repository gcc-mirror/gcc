// Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++23 } }

#include <atomic>
#include <stdfloat>

#if defined(__STDCPP_FLOAT16_T__)
void
test01()
{
  using A = std::atomic<std::float16_t>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, std::float16_t> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}
#endif

#if defined(__STDCPP_FLOAT32_T__)
void
test02()
{
  using A = std::atomic<std::float32_t>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, std::float32_t> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}
#endif

#if defined(__STDCPP_FLOAT64_T__)
void
test03()
{
  using A = std::atomic<std::float64_t>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, std::float64_t> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}
#endif

#if defined(__STDCPP_FLOAT128_T__)
void
test04()
{
  using A = std::atomic<std::float128_t>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, std::float128_t> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}
#endif

#if defined(__STDCPP_BFLOAT16_T__)
void
test05()
{
  using A = std::atomic<std::bfloat16_t>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, std::bfloat16_t> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}
#endif
