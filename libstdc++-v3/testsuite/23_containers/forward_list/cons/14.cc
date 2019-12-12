// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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

// 23.3.4.2 forward_list construction [forwardlist.cons]

#include <forward_list>
#include <scoped_allocator>

void test01()
{
  using namespace std;
  using list = forward_list<int>;
  using alloc_type = allocator<list>;
  forward_list<list, scoped_allocator_adaptor<alloc_type>> l;

  // Check for forward_list(size_type, const allocator_type&)
  l.emplace_front(1u);
}
