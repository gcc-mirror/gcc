// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <iterator>
#include <initializer_list>

void
test01()
{
  struct A { bool empty() const { return true; } };
  A a;
  std::empty(a);  // { dg-warning "ignoring return value" }
}

void
test02()
{
  int a[2];
  std::empty(a);  // { dg-warning "ignoring return value" }
}

void
test03()
{
  std::initializer_list<int> a{};
  std::empty(a);  // { dg-warning "ignoring return value" }
}
