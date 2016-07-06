// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <any>
#include <testsuite_hooks.h>

using std::any;
using std::any_cast;

struct X
{
  bool moved = false;
  bool moved_from = false;
  X() = default;
  X(const X&) = default;
  X(X&& x) : moved(true) { x.moved_from = true; }
};

void test01()
{
  X x;
  any a1;
  a1 = x;
  VERIFY(x.moved_from == false);
  any a2;
  a2 = std::move(x);
  VERIFY(x.moved_from == true);
  VERIFY(any_cast<X&>(a2).moved == true );
}

int main()
{
  test01();
}
