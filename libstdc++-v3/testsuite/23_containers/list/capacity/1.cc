// Copyright (C) 2001, 2004, 2005, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.2.2.2 list capacity [lib.list.capacity]

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// This test verifies the following.
//
// 23.2.2       bool empty() const
// 23.2.2       size_type size() const
// 23.2.2       iterator begin()
// 23.2.2       iterator end()
// 23.2.2.3     void push_back(const T&)
// 23.2.2       size_type max_size() const
// 23.2.2.2     void resize(size_type s, T c = T())
//
void
test01()
{
  std::list<int> list0101;
  VERIFY(list0101.empty());
  VERIFY(list0101.size() == 0);

  list0101.push_back(1);
  VERIFY(!list0101.empty());
  VERIFY(list0101.size() == 1);

  list0101.resize(3, 2);
  VERIFY(!list0101.empty());
  VERIFY(list0101.size() == 3);

  std::list<int>::iterator i = list0101.begin();
  VERIFY(*i == 1); ++i;
  VERIFY(*i == 2); ++i;
  VERIFY(*i == 2); ++i;
  VERIFY(i == list0101.end());

  list0101.resize(0);
  VERIFY(list0101.empty());
  VERIFY(list0101.size() == 0);
}

int
main()
{
  test01();
  return 0;
}

// vi:set sw=2 ts=2:
