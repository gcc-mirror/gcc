// { dg-do run { target c++11 } }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// 24.3.4 Iterator operations [iterator.operations]

#include <vector>
#include <list>
#include <iterator>
#include <testsuite_hooks.h>

void test01()
{
  std::vector<int> c(3);
  std::vector<int>::iterator i = c.begin(), j;

  j = std::next(i, 3);
  VERIFY( i == c.begin() );
  VERIFY( j == c.end() );
}

void test02()
{
  std::list<int> c(3);    
  std::list<int>::iterator i = c.begin(), j;

  j = std::next(i, 3);
  VERIFY( i == c.begin() );
  VERIFY( j == c.end() );
}

int main()
{
  test01();
  test02();
  return 0;
}
