// { dg-do run { target c++11 } }

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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
#include <forward_list>
#include <testsuite_hooks.h>

void
test01()
{
  std::forward_list<int> fl1{1, 2, 3};
  std::forward_list<int> fl2{4, 5, 6};

  auto before = fl1.before_begin();
  auto end = fl1.end();
  fl2.splice_after(fl2.before_begin(), std::move(fl1));

  VERIFY( before == fl1.before_begin() );
  VERIFY( end == fl1.end() );

  // no-op just to check that debug mode does not see any problem with it.
  fl1.splice_after(fl1.before_begin(), std::move(fl2),
		   fl2.before_begin(), fl2.begin());
}

int
main()
{
  test01();
  return 0;
}
