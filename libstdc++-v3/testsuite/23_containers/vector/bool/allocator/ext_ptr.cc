// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <vector>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using T = bool;

using __gnu_test::CustomPointerAlloc;

template class std::vector<T, CustomPointerAlloc<T>>;

void test01()
{
  typedef CustomPointerAlloc<T> alloc_type;
  typedef std::vector<T, alloc_type> test_type;
  test_type v;
  v.push_back(T());
  VERIFY( ++v.begin() == v.end() );
}

int main()
{
  test01();
}
