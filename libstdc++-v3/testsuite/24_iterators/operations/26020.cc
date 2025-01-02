// Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

// 24.3.4 Iterator operations

#include <iterator>
#include <list>
#include <testsuite_hooks.h>

//libstdc++/26020
void test01()
{
  using namespace std;

  list<int> ll;
  ll.push_back(1);

  list<int>::iterator it(ll.begin());

  advance(it, 0.5);

  VERIFY( it == ll.begin() );
}

int main()
{
  test01();
  return 0;
}
