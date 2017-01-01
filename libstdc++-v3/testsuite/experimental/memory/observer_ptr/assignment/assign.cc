// { dg-do run { target c++14 } }

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

#include <experimental/memory>
#include <testsuite_hooks.h>

using std::experimental::observer_ptr;

struct B {};
struct D : B {};

void test01()
{
  observer_ptr<int> a, b;
  a = b;
  VERIFY(a == b);
}

void test02()
{
  int x{};
  observer_ptr<int> a;
  observer_ptr<int> b{&x};
  VERIFY(a != b);
  a = b;
  VERIFY(a == b);
}

void test03()
{
  int x{};
  observer_ptr<const int> a;
  observer_ptr<int> b{&x};
  VERIFY(a != b);
  a = b;
  VERIFY(a == b);
}

void test04()
{
  D x{};
  observer_ptr<B> a;
  observer_ptr<D> b{&x};
  VERIFY(a != b);
  a = b;
  VERIFY(a == b);
}

constexpr bool test05_helper(observer_ptr<const int> a, 
                             observer_ptr<const int> b)
{
  a = b;
  return (a.get() == b.get());
}

void test05()
{
  static constexpr int x{};
  constexpr observer_ptr<const int> a;
  constexpr observer_ptr<const int> b{&x};
  constexpr bool assigned = test05_helper(a, b);
  VERIFY(assigned);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
