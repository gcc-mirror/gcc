// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

// 23.2.3.n forward_list xxx [lib.forward_list.xxx]

#include <forward_list>
#include <testsuite_hooks.h>

#include <algorithm>

bool test __attribute__((unused)) = true;

// This test verifies the following:
//   remove_if
void
test01()
{
  std::forward_list<int> fl ={0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  fl.remove_if(std::bind2nd(std::less<int>(),5));

  std::forward_list<int>::const_iterator pos = fl.cbegin();
  VERIFY(*pos == 5);
}

int
main()
{
  test01();
  return 0;
}
