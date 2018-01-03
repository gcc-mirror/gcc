// { dg-do run { target c++11 } }
// { dg-options "-Wno-deprecated" }

// 2007-10-27  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

// 20.4.1.1 allocator members

#include <ext/array_allocator.h>
#include <utility>
#include <testsuite_hooks.h>

void test01()
{
  typedef std::pair<int, char> pair_type;
  typedef std::tr1::array<pair_type, 3> array_type;
  array_type store;

  __gnu_cxx::array_allocator<pair_type, array_type> alloc1(&store);
  pair_type* ptp1 = alloc1.allocate(1);
  alloc1.construct(ptp1, 3, 'a');

  VERIFY( ptp1->first == 3 );
  VERIFY( ptp1->second == 'a' );

  alloc1.deallocate(ptp1, 1);
}

int main()
{
  test01();
  return 0;
}
