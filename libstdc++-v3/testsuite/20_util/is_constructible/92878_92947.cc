// { dg-do compile { target c++20 } }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <type_traits>

struct aggressive_aggregate
{
    int a;
    int b;
};

struct vicious_variation
{
    int a;
    int b = 42;
};

void test01()
{
  static_assert(std::is_constructible_v<aggressive_aggregate, int, int>);
  static_assert(std::is_constructible_v<aggressive_aggregate, int>);
  static_assert(std::is_constructible_v<aggressive_aggregate>);
  static_assert(std::is_default_constructible_v<aggressive_aggregate>);
  static_assert(std::is_trivially_default_constructible_v<
		aggressive_aggregate>);
  static_assert(std::is_constructible_v<vicious_variation, int, int>);
  static_assert(std::is_constructible_v<vicious_variation, int>);
  static_assert(std::is_constructible_v<vicious_variation>);
  static_assert(std::is_default_constructible_v<vicious_variation>);
  static_assert(!std::is_trivially_default_constructible_v<
		vicious_variation>);
}
