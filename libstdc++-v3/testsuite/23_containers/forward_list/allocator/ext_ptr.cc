// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#include <forward_list>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct T { int i; };
bool operator==(const T& l, const T& r) { return l.i == r.i; }
bool operator<(const T& l, const T& r) { return l.i < r.i; }

using __gnu_test::CustomPointerAlloc;

template class std::forward_list<T, CustomPointerAlloc<T>>;

void test01()
{
  typedef CustomPointerAlloc<T> alloc_type;
  typedef std::forward_list<T, alloc_type> test_type;
  test_type v;
  v.push_front(T());
  VERIFY( ++v.begin() == v.end() );
}

int main()
{
  test01();
}
