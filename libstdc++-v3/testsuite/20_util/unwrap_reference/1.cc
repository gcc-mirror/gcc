// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#include <type_traits>

#ifndef __cpp_lib_unwrap_ref
# error "Feature-test macro for unwrap_reference missing in <type_traits>"
#elif __cpp_lib_unwrap_ref != 201811L
# error "Feature-test macro for unwrap_reference has wrong value in <type_traits>"
#endif

template<typename T, typename U> struct expect_same;
template<typename T> struct expect_same<T, T> : std::true_type { };

template<typename T, typename U = T>
  constexpr bool check()
  {
    using std::unwrap_reference;
    using T2 = typename unwrap_reference<T>::type;
    static_assert(expect_same<T2, std::unwrap_reference_t<T2>>::value);
    return expect_same<T2, U>::value;
  }

void
test01()
{
  static_assert( check<int>() );
  static_assert( check<const int>() );
  static_assert( check<const int&>() );
  static_assert( check<const int*>() );
  static_assert( check<const int*&>() );

  // reference_wrapper types should get unwrapped:
  static_assert( check<std::reference_wrapper<int>, int&>() );
  static_assert( check<std::reference_wrapper<const int>, const int&>() );
  static_assert( check<std::reference_wrapper<long>, long&>() );

  // But not cv-qualified reference_wrapper types:
  static_assert( check<const std::reference_wrapper<int>>() );
  static_assert( check<volatile std::reference_wrapper<int>>() );
  static_assert( check<const volatile std::reference_wrapper<int>>() );

  // Or references to reference_wrapper types:
  static_assert( check<std::reference_wrapper<int>&>() );
  static_assert( check<std::reference_wrapper<int>&&>() );
  static_assert( check<const std::reference_wrapper<int>&>() );
}
