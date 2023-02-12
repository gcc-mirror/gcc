// Multimap iterator invalidation tests

// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

#include <debug/map>
#include <iterator>
#include <testsuite_hooks.h>
#include <utility>

using __gnu_debug::multimap;
using std::advance;

bool test = true;

// Assignment
void test01()
{
  multimap<int, int> v1;
  multimap<int, int> v2;

  v1.insert(std::make_pair(17, 42));

  multimap<int, int>::iterator start = v1.begin();
  multimap<int, int>::iterator finish = v1.end();
  VERIFY(start._M_dereferenceable());
  VERIFY(!finish._M_dereferenceable() && !finish._M_singular());

  v1 = v2;
  VERIFY(start._M_singular());
  VERIFY(!finish._M_dereferenceable() && !finish._M_singular());
}

int main()
{
  test01();
  return 0;
}
