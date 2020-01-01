// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct T { int i; };

struct hash
{
  std::size_t operator()(const T t) const noexcept
  { return t.i; }
};

struct equal_to
{
  bool operator()(const T& lhs, const T& rhs) const noexcept
  { return lhs.i == rhs.i; }
};

using __gnu_test::propagating_allocator;

void test01()
{
  typedef propagating_allocator<T, false> alloc_type;
  typedef std::unordered_multiset<T, hash, equal_to, alloc_type> test_type;
  test_type v1(alloc_type(1));
  v1.insert(T());
  test_type v2(v1);
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(0 == v2.get_allocator().get_personality());
}

void test02()
{
  typedef propagating_allocator<T, true> alloc_type;
  typedef std::unordered_multiset<T, hash, equal_to, alloc_type> test_type;
  test_type v1(alloc_type(1));
  v1.insert(T());
  test_type v2(v1);
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(1 == v2.get_allocator().get_personality());
}

void test03()
{
  typedef propagating_allocator<T, true> alloc_type;
  typedef std::unordered_multiset<T, hash, equal_to, alloc_type> test_type;
  test_type v1(alloc_type(1));
  v1.insert(T());
  test_type v2(v1, alloc_type(2));
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(2 == v2.get_allocator().get_personality());
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
