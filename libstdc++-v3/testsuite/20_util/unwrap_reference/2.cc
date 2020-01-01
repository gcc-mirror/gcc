// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

template<typename T, typename U> struct expect_same;
template<typename T> struct expect_same<T, T> : std::true_type { };

template<typename T, typename U = std::decay_t<T>>
  constexpr bool check()
  {
    using T2 = typename std::unwrap_ref_decay<T>::type;
    static_assert(expect_same<T2, std::unwrap_ref_decay_t<T>>::value);
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

  // reference_wrapper types (including cv and references) get unwrapped:
  static_assert( check<std::reference_wrapper<int>, int&>() );
  static_assert( check<std::reference_wrapper<int>&, int&>() );
  static_assert( check<const std::reference_wrapper<int>, int&>() );
  static_assert( check<const std::reference_wrapper<int>&, int&>() );
  static_assert( check<std::reference_wrapper<const int>, const int&>() );
  static_assert( check<std::reference_wrapper<const int>&, const int&>() );
  static_assert( check<std::reference_wrapper<long>, long&>() );
}
