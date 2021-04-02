// { dg-do run { target c++14 } }

// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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
  X(X&&) { moved = true; }
};

struct X2
{
  X2() = default;
  X2(const X2&) { copied = true; }
  X2(X2&&) noexcept { moved = true; }
};

void test01()
{
  moved = false;
  X x;
  any a1(x);
  VERIFY(moved == false);
  any a2(std::move(x));
  VERIFY(moved == true);
}

void test02()
{
  moved = false;
  X x;
  any a1(x);
  VERIFY(moved == false);
  copied = false;
  any a2(std::move(a1));
  VERIFY(copied == false);
}

void test03()
{
  moved = false;
  X2 x;
  any a1(x);
  VERIFY(moved == false);
  copied = false;
  any a2(std::move(a1));
  VERIFY(copied == false);
  VERIFY(moved == true);
}

int main()
{
  test01();
  test02();
  test03();
}
