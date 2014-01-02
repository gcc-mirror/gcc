// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

// Check that iterators ownership is correctly manage on swap.
#include <vector>
#include <forward_list>
#include <iostream>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  std::forward_list<int> fl1{1, 3, 5};
  std::forward_list<int> fl2{2, 4, 6};

  std::vector<std::forward_list<int>::iterator> fl1_its;
  fl1_its.push_back(fl1.before_begin());
  for (std::forward_list<int>::iterator it = fl1.begin();
       it != fl1.end(); ++it)
    {
      fl1_its.push_back(it);
    }
  fl1_its.push_back(fl1.end());

  std::vector<std::forward_list<int>::iterator> fl2_its;
  fl2_its.push_back(fl2.before_begin());
  for (std::forward_list<int>::iterator it = fl2.begin();
       it != fl2.end(); ++it)
    {
      fl2_its.push_back(it);
    }
  fl2_its.push_back(fl2.end());

  fl1.swap(fl2);

  auto fit = fl1.before_begin();
  // before-begin iterator is not transfered:
  // TODO: Validate with LWG group how before begin should be
  // treated.
  VERIFY( fit == fl1_its[0] );
  // All other iterators are, even paste-the-end ones:
  for (size_t i = 1; i != fl2_its.size(); ++i)
  {
    VERIFY( ++fit == fl2_its[i] );
  }

  fit = fl2.before_begin();
  // TODO: Validate with LWG group how before begin should be
  // treated.
  VERIFY( fit == fl2_its[0] );
  for (size_t i = 1; i != fl1_its.size(); ++i)
  {
    VERIFY( ++fit == fl1_its[i] );
  }
}

int
main()
{
  test01();
  return 0;
}
