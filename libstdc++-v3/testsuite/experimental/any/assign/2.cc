// { dg-do run { target c++14 } }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <experimental/any>
#include <testsuite_hooks.h>

using std::experimental::any;
using std::experimental::any_cast;

bool moved = false;
bool copied = false;


struct X
{
  X() = default;
  X(const X&) { copied = true; }
  X(X&& x) { moved = true; }
};

struct X2
{
  X2() = default;
  X2(const X2&) { copied = true; }
  X2(X2&& x) noexcept { moved = true; }
};

void test01()
{
  moved = false;
  X x;
  any a1;
  a1 = x;
  VERIFY(moved == false);
  any a2;
  copied = false;
  a2 = std::move(x);
  VERIFY(moved == true);
  VERIFY(copied == false);
}

void test02()
{
  moved = false;
  X x;
  any a1;
  a1 = x;
  VERIFY(moved == false);
  any a2;
  copied = false;
  a2 = std::move(a1);
  VERIFY(moved == false);
  VERIFY(copied == false);
}

void test03()
{
  moved = false;
  X2 x;
  any a1;
  a1 = x;
  VERIFY(copied && moved);
  any a2;
  moved = false;
  copied = false;
  a2 = std::move(a1);
  VERIFY(moved == true);
  VERIFY(copied == false);
 }

int main()
{
  test01();
  test02();
  test03();
}
