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

void
test01()
{
  // PR libstdc++/96042
  using V = std::ranges::iota_view<long long, long long>;

  // In strict -std=c++20 mode there is no integer wider than long long,
  // so V's difference type is an integer-class type, [iterator.concept.winc].
  // In practice this is either __int128 or __detail::__max_diff_type.
  using D = std::ranges::range_difference_t<V>;
  // Ensure that numeric_limits is correctly specialized for the type.
  using L = std::numeric_limits<D>;
  static_assert( L::is_specialized );
  static_assert( L::is_signed );
  static_assert( L::is_integer );
  static_assert( L::is_exact );
  static_assert( L::digits > std::numeric_limits<long long>::digits );
  static_assert( L::digits10 == static_cast<int>(L::digits * 0.30103) );
  static_assert( L::min() == (D(1) << L::digits) );
  static_assert( L::max() == ~L::min() );
  static_assert( L::lowest() == L::min() );
}

#ifdef __SIZEOF_INT128__
void
test02()
{
  // When the target supports __int128 it can be used in iota_view
  // even in strict mode where !integral<__int128>.
  using V = std::ranges::iota_view<__int128, __int128>;
  using D = std::ranges::range_difference_t<V>; // __detail::__max_diff_type
  using L = std::numeric_limits<D>;
  static_assert( L::is_specialized );
  static_assert( L::is_signed );
  static_assert( L::is_integer );
  static_assert( L::is_exact );
  static_assert( L::digits > std::numeric_limits<long long>::digits );
  static_assert( L::digits10 == static_cast<int>(L::digits * 0.30103) );
  static_assert( L::min() == (D(1) << L::digits) );
  static_assert( L::max() == ~L::min() );
  static_assert( L::lowest() == L::min() );
}
#endif
