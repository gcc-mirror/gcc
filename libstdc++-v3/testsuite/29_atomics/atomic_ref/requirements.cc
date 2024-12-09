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
#include <type_traits>

template <class T>
void
test_generic()
{
  using A = std::atomic_ref<T>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<typename A::value_type, std::remove_cv_t<T>> );
  static_assert( !requires { typename A::difference_type; } );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

template <class T>
void
test_integral()
{
  using A = std::atomic_ref<T>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<typename A::value_type, std::remove_cv_t<T>> );
  static_assert( std::is_same_v<typename A::difference_type, typename A::value_type> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

template <class T>
void
test_floating_point()
{
  using A = std::atomic_ref<T>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<typename A::value_type, std::remove_cv_t<T>> );
  static_assert( std::is_same_v<typename A::difference_type, typename A::value_type> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

template <class T>
void
test_pointer()
{
  using A = std::atomic_ref<T>;
  static_assert( std::is_standard_layout_v<A> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( std::is_trivially_destructible_v<A> );
  static_assert( std::is_same_v<typename A::value_type, std::remove_cv_t<T>> );
  static_assert( std::is_same_v<typename A::difference_type, std::ptrdiff_t> );
  static_assert( std::is_nothrow_copy_constructible_v<A> );
  static_assert( !std::is_copy_assignable_v<A> );
  static_assert( !std::is_move_assignable_v<A> );
}

int
main()
{
  struct X { int c; };
  test_generic<X>();
  test_generic<const X>();
  test_generic<volatile X>();
  test_generic<const volatile X>();

  // atomic_ref excludes (cv) `bool` from the set of integral types
  test_generic<bool>();
  test_generic<const bool>();
  test_generic<volatile bool>();
  test_generic<const volatile bool>();

  test_integral<int>();
  test_integral<const int>();
  test_integral<volatile int>();
  test_integral<const volatile int>();

  test_floating_point<double>();
  test_floating_point<const double>();
  test_floating_point<volatile double>();
  test_floating_point<const volatile double>();

  test_pointer<int*>();
  test_pointer<int* const>();
  test_pointer<int* volatile>();
  test_pointer<int* const volatile>();
}
