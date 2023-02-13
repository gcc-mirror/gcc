// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=c++2a" }
// { dg-do compile { target c++2a } }

#include <ranges>
#include <limits>

template<typename T, typename U>
constexpr bool
equal(T t, U u) requires std::same_as<T, U>
{
  return t == u;
}

template<typename W, typename S = std::make_unsigned_t<W>>
void
test_integer_iota()
{
  using std::numeric_limits;

  using V = std::ranges::iota_view<W, W>;
  static_assert( std::ranges::sized_range<V> );

  constexpr V zero(0, 0);
  static_assert( equal(zero.size(), (S)0) );

  constexpr V min(numeric_limits<W>::min(),
		  numeric_limits<W>::min());
  static_assert( equal(min.size(), (S)0) );

  constexpr V max(numeric_limits<W>::max(),
		  numeric_limits<W>::max());
  static_assert( equal(max.size(), (S)0) );

  constexpr V minmax(numeric_limits<W>::min(),
		     numeric_limits<W>::max());
  if constexpr (sizeof(W) < sizeof(S))
  {
    using S2 = std::make_unsigned_t<W>;
    static_assert( equal(minmax.size(), (S)numeric_limits<S2>::max()) );
  }
  else
    static_assert( equal(minmax.size(), numeric_limits<S>::max()) );

  constexpr V pospos(20, 22);
  static_assert( equal(pospos.size(), (S)2) );

  if constexpr (std::numeric_limits<W>::is_signed)
  {
    constexpr V negneg(-20, -2);
    static_assert( equal(negneg.size(), (S)18) );

    constexpr V negpos(-20, 22);
    static_assert( equal(negpos.size(), (S)42) );
  }
}

void
test01()
{
  test_integer_iota<signed char, unsigned int>();
  test_integer_iota<signed short, unsigned int>();
  test_integer_iota<signed int>();
  test_integer_iota<signed long>();
  test_integer_iota<signed long long>();
  test_integer_iota<unsigned char, unsigned int>();
  test_integer_iota<unsigned short, unsigned int>();
  test_integer_iota<unsigned int>();
  test_integer_iota<unsigned long>();
  test_integer_iota<unsigned long long>();

#ifdef __SIZEOF_INT128__
  // When the target supports __int128 it can be used in iota_view
  // even in strict mode where !integral<__int128>.
  // Specify the size type explicitly, because make_unsigned_t<__int128>
  // is undefined when !integral<__int128>.
  test_integer_iota<__int128, unsigned __int128>();
  test_integer_iota<unsigned __int128, unsigned __int128>();
#endif
}

constexpr int arr[3] = { 1, 2, 3 };

void
test02()
{
  constexpr auto v = std::views::iota(std::begin(arr), std::end(arr));
  static_assert( equal(v.size(), std::make_unsigned_t<std::ptrdiff_t>(3)) );

  constexpr auto vv = std::views::iota(v.begin(), v.end());
  constexpr auto vvsz = vv.size();
  static_assert( ! std::numeric_limits<decltype(vvsz)>::is_signed );
  static_assert( vvsz == 3 );
}
