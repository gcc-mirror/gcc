// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <atomic>

void
test01()
{
  using A = std::atomic<float>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, float> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}

void
test02()
{
  using A = std::atomic<double>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, double> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}

void
test03()
{
  using A = std::atomic<long double>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( !std::is_trivially_default_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, long double> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_constructible_v<A> );
  static_assert( !std::is_move_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
  static_assert( !std::is_assignable_v<volatile A&, const A&> );
}
