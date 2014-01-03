// { dg-options "-std=gnu++0x" }
// { dg-require-debug-mode "" }
// { dg-do run { xfail *-*-* } }

// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#include <forward_list>
#include <testsuite_allocator.h>

void
test01()
{
  typedef __gnu_test::uneq_allocator<int> alloc_type;

  std::forward_list<int, alloc_type> fl1({1, 2, 3}, alloc_type(1));
  std::forward_list<int, alloc_type> fl2({1, 2, 3}, alloc_type(2));

  fl1.splice_after(fl1.before_begin(), fl2, fl2.begin());
}

int
main()
{
  test01();
  return 0;
}
