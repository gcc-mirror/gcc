// { dg-do run { target c++11 } }

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_test::propagating_allocator;

void test01()
{
  typedef propagating_allocator<int, true> alloc_type;
  alloc_type alloc(5);

  std::vector<int, alloc_type> v(10u, 1, alloc);
  v.reserve(100);
  VERIFY( v.size() < v.capacity() );
  v.shrink_to_fit();
  VERIFY( v.size() == v.capacity() );
  VERIFY( v.get_allocator().get_personality() == alloc.get_personality() );
}

void test02()
{
  typedef propagating_allocator<int, false> alloc_type;
  alloc_type alloc(5);

  std::vector<int, alloc_type> v(10u, 1, alloc);
  v.reserve(100);
  VERIFY( v.size() < v.capacity() );
  v.shrink_to_fit();
  VERIFY( v.size() == v.capacity() );
  VERIFY( v.get_allocator().get_personality() == alloc.get_personality() );
}

int main()
{
  test01();
  test02();
  return 0;
}
