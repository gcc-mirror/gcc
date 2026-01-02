// Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

// { dg-do compile { target c++26 } }

#include <compare>

#if __cpp_lib_type_order != 202506L
# error "__cpp_lib_type_order != 202506"
#endif

static_assert (std::is_same_v <decltype (std::type_order <int, int>::value),
			       const std::strong_ordering>);
static_assert (std::is_same_v <decltype (std::type_order_v <char, short>),
			       const std::strong_ordering>);
struct S;
struct T;
template <typename T>
struct U
{
};
typedef int int2;
struct V {};
namespace
{
  struct W {};
}

template <typename T, typename U>
struct eq
{
  constexpr eq ()
  {
    static_assert (std::type_order <T, U>::value == std::strong_ordering::equal);
    static_assert (std::type_order <U, T>::value == std::strong_ordering::equal);
    static_assert (std::type_order_v <T, U> == std::strong_ordering::equal);
    static_assert (std::type_order_v <U, T> == std::strong_ordering::equal);
  }
};
template <typename T, typename U>
struct ne
{
  constexpr ne ()
  {
    static_assert (std::type_order <T, U>::value != std::strong_ordering::equal);
    static_assert (std::type_order <U, T>::value != std::strong_ordering::equal);
    static_assert (std::type_order <T, U>::value == std::strong_ordering::greater
		   ? std::type_order <U, T>::value == std::strong_ordering::less
		   : std::type_order <U, T>::value == std::strong_ordering::greater);
    static_assert (std::type_order_v <T, U> != std::strong_ordering::equal);
    static_assert (std::type_order_v <U, T> != std::strong_ordering::equal);
    static_assert (std::type_order_v <T, U> == std::strong_ordering::greater
		   ? std::type_order_v <U, T> == std::strong_ordering::less
		   : std::type_order_v <U, T> == std::strong_ordering::greater);
  }
};

constexpr eq <void, void> a;
constexpr eq <const void, const void> b;
constexpr eq <int, int> c;
constexpr eq <long int, long int> d;
constexpr eq <const volatile unsigned, const volatile unsigned> e;
constexpr eq <S, S> f;
constexpr eq <U <int>, U <int>> g;
constexpr eq <unsigned[2], unsigned[2]> h;
constexpr eq <int, int2> i;
constexpr eq <int (*) (int, long), int (*) (int, long)> j;
constexpr ne <int, long> k;
constexpr ne <const int, int> l;
constexpr ne <S, T> m;
constexpr ne <int &, int &&> n;
constexpr ne <U <S>, U <T>> o;
constexpr ne <U <short>, U <char>> p;
static_assert (std::type_order_v <S, T> != std::strong_ordering::less
	       || std::type_order_v <T, V> != std::strong_ordering::less
	       || std::type_order_v <S, V> == std::strong_ordering::less);
constexpr ne <int (*) (int, long), int (*) (int, int)> q;
constexpr eq <W, W> r;
constexpr ne <V, W> s;
constexpr eq <U <W>, U <W>> t;
constexpr ne <U <V>, U <W>> u;
