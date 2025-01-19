// Copyright (C) 2014-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//
// { dg-do run { target c++11 xfail *-*-* } }

#include <debug/unordered_set>

#include <testsuite_allocator.h>

void test01()
{
  typedef __gnu_test::uneq_allocator<int> alloc_type;
  typedef __gnu_debug::unordered_multiset<int, std::hash<int>,
					  std::equal_to<int>,
					  alloc_type> test_type;

  test_type v1(alloc_type(1));
  v1.insert(0);
  auto it = v1.begin();

  test_type v2(std::move(v1), alloc_type(2));

  VERIFY( it == v2.begin() ); // Error, it is singular
}

int main()
{
  test01();
  return 0;
}
