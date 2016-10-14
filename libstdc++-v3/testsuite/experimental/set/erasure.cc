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

#include <experimental/set>
#include <testsuite_hooks.h>

auto is_odd = [](const int i) { return i % 2 != 0; };

void
test01()
{
  std::set<int> s{ 10, 11, 12, 14, 15, 17, 18, 19 };
  std::experimental::erase_if(s, is_odd);
  std::set<int> t{ 10, 12, 14, 18 };
  VERIFY( s == t );
}

void
test02()
{
  std::multiset<int> ms{ 20, 21, 22, 22, 23, 23, 24, 25 };
  std::experimental::erase_if(ms, is_odd);
  std::multiset<int> t{ 20, 22, 22, 24 };
  VERIFY( ms == t );
}

int
main()
{
  test01();
  test02();

  return 0;
}
