// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

#include <algorithm>
#include <iterator>
#include <forward_list>
#include <list>
#include <vector>
#include <testsuite_hooks.h>

template<typename Container>
void
test(Container& c)
{
  int size = std::distance(c.begin(), c.end());
  for (int i=0; i < size; ++i)
  {
    auto first = c.begin(), middle = std::next(first, i), last = c.end();
    auto r = std::rotate(first, middle, last);
    auto expected = std::next(first, std::distance(middle, last));
    VERIFY( r == expected );
  }
}

void
test01()
{
  // test random access iterators
  std::vector<int> v{ 0, 1, 2, 3, 4 };
  test(v);
  v.push_back(5);
  test(v);
}

void
test02()
{
  // test bidirectional iterators
  std::list<int> l{ 0, 1, 2, 3, 4 };
  test(l);
  l.push_back(5);
  test(l);
}

void
test03()
{
  // test forward iterators
  std::forward_list<int> l{ 0, 1, 2, 3, 4 };
  test(l);
  l.push_front(5);
  test(l);
}

int
main()
{
  test01();
  test02();
  test03();
}
