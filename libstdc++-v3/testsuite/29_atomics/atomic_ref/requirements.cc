// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <atomic>

void
test01()
{
  struct X { int c; };
  using A = std::atomic_ref<X>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, X> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

void
test02()
{
  using A = std::atomic_ref<int>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, int> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

void
test03()
{
  using A = std::atomic_ref<double>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, double> );
  static_assert( std::is_same_v<A::difference_type, A::value_type> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

void
test04()
{
  using A = std::atomic_ref<int*>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<A::value_type, int*> );
  static_assert( std::is_same_v<A::difference_type, std::ptrdiff_t> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}
