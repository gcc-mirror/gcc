// { dg-do run { target c++11 } }

// Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

// 23.3.4.2 forward_list construction [forwardlist.cons]

#include <forward_list>

bool fail = false;

struct A
{
  A() = default;
  A(const A&) { if (fail) throw fail; }
};

void test01()
{
  typedef std::forward_list<A> list;

  list l(2);
  A from[2];
  fail = true;
  // Check existing elements are assigned to, instead of creating new ones.
  // This is QoI, not required by the standard.
  l.assign(from, from+2);
  l.assign(2, from[0]);
}

int main()
{
  test01();
}
