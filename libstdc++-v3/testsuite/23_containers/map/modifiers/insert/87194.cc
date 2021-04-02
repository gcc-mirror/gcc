// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <map>
#include <iterator>
#include <testsuite_hooks.h>

struct S
{
  operator std::pair<const int, int>() &&
  { int i = val; val = 0; return {i, 0}; }

  int val;
};

void
test01()
{
  S a[3] = { {1}, {2}, {3} };
  std::map<int, int> s;
  s.insert(std::make_move_iterator(a), std::make_move_iterator(a+3));
  VERIFY( s.size() == 3 );
  VERIFY( s.find(0) == s.end() );
}

int
main()
{
  test01();
}
