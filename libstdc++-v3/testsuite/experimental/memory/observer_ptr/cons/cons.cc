// { dg-do run { target c++14 } }

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
#include <utility>

using std::experimental::observer_ptr;

struct B {};
struct D : B {};

void test01()
{
  observer_ptr<int> a;
  VERIFY(!a);
  observer_ptr<int> b{nullptr};
  VERIFY(!b);
}

void test02()
{
  int x{};
  observer_ptr<int> a{&x};
  observer_ptr<int> b{a};
  VERIFY(a == b);
}

void test03()
{
  observer_ptr<int> a;
  observer_ptr<const int> b{a};
  VERIFY(a == b);
}

void test04()
{
  D x{};
  observer_ptr<D> a{&x};
  observer_ptr<B> b{a};
  VERIFY(a == b);
}

void test05()
{
  D x{};
  observer_ptr<D> a{&x};
  observer_ptr<B> b{std::move(a)};
  VERIFY(a == b);
}

void test06()
{
  static constexpr D x{};
  constexpr observer_ptr<const D> a{&x};
  constexpr observer_ptr<const B> b{std::move(a)};
  VERIFY(a == b);
  constexpr observer_ptr<const B> c{a};
  VERIFY(a == b && a == c && b == c);
  constexpr observer_ptr<int> d;
  constexpr observer_ptr<int> e{nullptr};
  VERIFY(!d);
  VERIFY(!e);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
