// { dg-do run { target c++14 } }

// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
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
  VERIFY(a == b);
  swap(a, b);
  VERIFY(a == b);
}

void test02()
{
  int x{};
  observer_ptr<int> a;
  observer_ptr<int> b{&x};
  VERIFY(!a);
  VERIFY(b);
  swap(a, b);
  VERIFY(a);
  VERIFY(!b);
}

void test03()
{
  int x[2]{1,2};
  observer_ptr<int> a{&x[0]};
  observer_ptr<int> b{&x[1]};
  VERIFY(*a == 1);
  VERIFY(*b == 2);
  swap(a, b);
  VERIFY(*a == 2);
  VERIFY(*b == 1);
}


int main()
{
  test01();
  test02();
  test03();
}
