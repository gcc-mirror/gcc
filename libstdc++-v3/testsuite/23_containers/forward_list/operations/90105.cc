// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <forward_list>
#include <testsuite_hooks.h>

// PR libstdc++/90105 - std::forward_list::sort() is not "stable"

struct X
{
  int key;
  int val;
};

bool operator<(const X& l, const X& r)
{ return l.key < r.key; }

bool operator==(const X& l, const X& r)
{ return l.key == r.key && l.val == r.val; }

void
test01()
{
  std::forward_list<X> l{ {1, 1}, {2, 2}, {1, 3}, {0, 4}, {2, 5}, {0, 6} };
  l.sort();
  std::forward_list<X> exp{ {0, 4}, {0, 6}, {1, 1}, {1, 3}, {2, 2}, {2, 5} };
  VERIFY( l == exp );
}

void
test02()
{
  std::forward_list<X> l{ {1, 1}, {1, 2}, {1, 3}, {1, 4}, {1, 5}, {1, 6} };
  const std::forward_list<X> exp = l;
  l.sort();
  VERIFY( l == exp );
}

int
main()
{
  test01();
  test02();
}
