// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <utility>

void test01()
{
  std::pair<float, int> p;

  float&& pfirst __attribute__((unused)) = std::get<float>(std::move(p));
  int&&  psecond __attribute__((unused)) = std::get<int>(std::move(p));

  const std::pair<float, int> cp;

  const float&& cpfirst __attribute__((unused)) =
    std::get<float>(std::move(cp));
  const int&&  cpsecond __attribute__((unused)) =
    std::get<int>(std::move(cp));
}

// PR libstdc++/121745 return of get(pair<_Up, _Tp>&& __p) may be ill-formed
void
test_pr121745(std::pair<float&, int&> p)
{
  float& pfirst = std::get<float&>(std::move(p));
  int& psecond  = std::get<int&>(std::move(p));

  const auto& p2 = p;
  float& p2first = std::get<float&>(std::move(p2));
  int& p2second  = std::get<int&>(std::move(p2));
}

template<typename T, typename Pair>
using get_t = decltype(std::get<T>(std::declval<Pair>()));

// Check that get<T>(Pair) returns Ret
template<typename T, typename Pair, typename Ret>
constexpr bool verify = std::is_same<get_t<T, Pair>, Ret>::value;

template<typename T1, typename T2>
void
check()
{
  // Overloads for accessing first member
  static_assert( verify<T1, std::pair<T1, T2>&, T1&>,
		 "T1& get(pair<T1, T2>&)" );
  static_assert( verify<T1, const std::pair<T1, T2>&, const T1&>,
		 "const T1& get(const pair<T1, T2>&)" );
  static_assert( verify<T1, std::pair<T1, T2>&&, T1&&>,
		 "T1&& get(pair<T1, T2>&&)" );
  static_assert( verify<T1, const std::pair<T1, T2>&&, const T1&&>,
		 "const T1&& get(const pair<T1, T2>&&)" );

  // Overloads for accessing second member
  static_assert( verify<T2, std::pair<T1, T2>&, T2&>,
		 "T2& get(pair<T1, T2>&)" );
  static_assert( verify<T2, const std::pair<T1, T2>&, const T2&>,
		 "const T2& get(const pair<T1, T2>&)" );
  static_assert( verify<T2, std::pair<T1, T2>&&, T2&&>,
		 "T2&& get(pair<T1, T2>&&)" );
  static_assert( verify<T2, const std::pair<T1, T2>&&, const T2&&>,
		 "const T2&& get(const pair<T1, T2>&&)" );
}

void
test_all()
{
  check<float, int>();
  check<float&, int&>();
  check<float&&, int&&>();
}
