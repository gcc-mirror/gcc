// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <vector>
#include <memory>
#include <testsuite_allocator.h>

using T = bool;

using __gnu_test::SimpleAllocator;

template class std::vector<T, SimpleAllocator<T>>;

void test01()
{
  typedef SimpleAllocator<T> alloc_type;
  typedef std::allocator_traits<alloc_type> traits_type;
  typedef std::vector<T, alloc_type> test_type;
  test_type v(alloc_type{});
  v.push_back(T());
}
