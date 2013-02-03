// { dg-options "-std=gnu++0x" }

// 2008-06-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2013 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <testsuite_containers.h>

int main()
{
  typedef std::unordered_multiset<int> 		test_type;
  typedef typename test_type::value_type	value_type;
  value_type v(1);
  __gnu_test::forward_members_unordered<test_type> test(v);
  return 0;
}
