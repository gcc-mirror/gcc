// { dg-do run { target c++14 } }

// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

#include <experimental/memory>
#include <testsuite_hooks.h>

using std::experimental::observer_ptr;

constexpr void test01()
{
  observer_ptr<int> a, b;
  VERIFY(a == b);
}

constexpr void test02()
{
  int x[2]{};
  observer_ptr<int> a{&x[0]};
  observer_ptr<int> b{&x[1]};
  VERIFY(a != b);
  VERIFY(a < b);
  VERIFY(a <= b);
  VERIFY(b >= a);
  VERIFY(b > a);
}

constexpr void test03()
{
  int x{};
  observer_ptr<int> a{&x};
  observer_ptr<int> b{&x};
  VERIFY(a == b);
}

int x[2]{};

constexpr void test04()
{
  constexpr observer_ptr<const int> a{&x[0]};
  constexpr observer_ptr<const int> b{&x[1]};
  VERIFY(a != b);
  VERIFY(a < b);
  VERIFY(a <= b);
  VERIFY(b >= a);
  VERIFY(b > a);
}

constexpr void test05()
{
  constexpr observer_ptr<const int> a{&x[0]};
  constexpr observer_ptr<const int> b{&x[0]};
  VERIFY(a == b);
}

constexpr bool all_tests()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return true;
}

int main()
{
  all_tests();
  static_assert( all_tests(), "LWG 4295 - relops should be constexpr" );
}
