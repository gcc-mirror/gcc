// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

#include <span>

template<typename T, int N, typename U>
constexpr bool is_static_span(const U&)
{
  return std::is_same_v<std::span<T, N>, U> && N != std::dynamic_extent;
}

template<typename T, typename U>
constexpr bool is_dynamic_span(const U&)
{
  return std::is_same_v<std::span<T>, U>;
}

struct Range
{
  float* begin() const;
  float* end() const;
};

void
test01()
{
  const char c[] = "";
  int i[2]{};
  std::array<long, 3> a;
  Range r;

  std::span s1(c);
  static_assert( is_static_span<const char, 1>(s1) );

  std::span s2(i);
  static_assert( is_static_span<int, 2>(s2) );

  std::span s3(a);
  static_assert( is_static_span<long, 3>(s3) );

  std::span s4(const_cast<const std::array<long, 3>&>(a));
  static_assert( is_static_span<const long, 3>(s4) );

  std::span s5(std::begin(i), std::end(i));
  static_assert( is_dynamic_span<int>(s5) );

  std::span s6(std::cbegin(i), std::cend(i));
  static_assert( is_dynamic_span<const int>(s6) );

  std::span s7(r);
  static_assert( is_dynamic_span<float>(s7) );

  std::span s8(s1);
  static_assert( is_static_span<const char, 1>(s8) );

  std::span s9(s2);
  static_assert( is_static_span<int, 2>(s9) );

  std::span s10(const_cast<const std::span<int, 2>&>(s2));
  static_assert( is_static_span<int, 2>(s10) );

  std::span s11(s5);
  static_assert( is_dynamic_span<int>(s11) );

  std::span s12(const_cast<const std::span<int>&>(s5));
  static_assert( is_dynamic_span<int>(s12) );

  std::span s13(a.data(), std::integral_constant<size_t, 3>{});
  static_assert( is_static_span<long, 3>(s13) );

  std::span s14(a.data(), true);
  static_assert( is_dynamic_span<long>(s14) );

  std::span s15(a.data(), std::true_type{});
  static_assert( is_dynamic_span<long>(s15) );

#if __glibcxx_constant_wrapper
  auto c5 = std::constant_wrapper<5>{};
  std::span s16(a.data(), c5);
  static_assert( is_static_span<long, 5>(s16) );

  std::span s17(a.data(), std::cw<4>);
  static_assert( is_static_span<long, 4>(s17) );

  std::span s18(a.data(), std::cw<true>);
  static_assert( is_dynamic_span<long>(s18) );
#endif
}
