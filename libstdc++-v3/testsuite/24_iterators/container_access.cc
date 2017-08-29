// { dg-do run { target c++1z } }
// { dg-options "-std=gnu++17" }

// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

// 24.8, container access [iterator.container]

#include <iterator>
#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  int i[42];
  VERIFY(std::data(i) == i);
  VERIFY(std::size(i) == 42);
  VERIFY(!std::empty(i));
}

void
test02()
{
  static constexpr int i[42]{};
  constexpr auto d = std::data(i);
  static_assert(d == i);
  constexpr auto s = std::size(i);
  static_assert(s == 42);
  constexpr auto e = std::empty(i);
  static_assert(!e);
}

void
test03()
{
  std::initializer_list<int> il{1,2,3};
  VERIFY(std::data(il) == il.begin());
  VERIFY(std::size(il) == 3);
  VERIFY(!std::empty(il));
  std::initializer_list<int> il2{};
  VERIFY(std::size(il2) == 0);
  VERIFY(std::empty(il2));
  static constexpr std::initializer_list<int> il3{1,2,3};
  constexpr auto d = std::data(il3);
  static_assert(d == il3.begin());
  constexpr auto s = std::size(il3);
  static_assert(s == 3);
  constexpr auto e = std::empty(il3);
  static_assert(!e);
}

void
test04()
{
  std::vector<int> v{1,2,3};
  VERIFY(std::data(v) == v.data());
  VERIFY(std::size(v) == v.size());
  VERIFY(!std::empty(v));
  std::vector<int> v2{};
  VERIFY(std::size(v2) == v2.size());
  VERIFY(std::empty(v2));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
