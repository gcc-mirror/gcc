// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

#include <type_traits>
#include <array>
#include <utility>

template<typename... Args>
constexpr
std::array<typename std::common_type<Args...>::type, 
  sizeof...(Args)>
make_array(Args&&... args)  // { dg-error "invalid use" }
{
  typedef typename std::common_type<Args...>::type CT;
  return std::array<CT, sizeof...(Args)>{static_cast<CT>
      (std::forward<Args>(args))...};
}

void test01()
{
  constexpr auto a1 = make_array(0);
  constexpr auto a2 = make_array(0, 1.2);
  constexpr auto a3 = make_array(5, true, 3.1415f, 'c');
  
  int i{};
  double d{1.2};
  float f{3.1415f};
  
  auto b1 = make_array(i);
  auto b2 = make_array(i, 1.2);
  auto b3 = make_array(i, d);
  auto b4 = make_array(0, d);
  auto b5 = make_array(i, true, f, 'c');

  static_assert(std::is_same<decltype(a1), const std::array<int, 1>>(), "");
  static_assert(std::is_same<decltype(a2), const std::array<double, 2>>(), "");
  static_assert(std::is_same<decltype(a3), const std::array<float, 4>>(), "");

  static_assert(std::is_same<decltype(b1), std::array<int, 1>>(), "");
  static_assert(std::is_same<decltype(b2), std::array<double, 2>>(), "");
  static_assert(std::is_same<decltype(b3), std::array<double, 2>>(), "");
  static_assert(std::is_same<decltype(b4), std::array<double, 2>>(), "");
  static_assert(std::is_same<decltype(b5), std::array<float, 4>>(), "");
}

void test02()
{
  make_array(); // { dg-error "no matching function" }
}
// { dg-prune-output "substitution" }
// { dg-prune-output "include" }
