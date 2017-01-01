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

#include <memory>
#include <limits>
#include <type_traits>
#include <testsuite_hooks.h>

struct X { };

template<typename T>
struct maxsize_allocator
{
  typedef T value_type;
  typedef unsigned size_type;

  size_type max_size() const { return 100; }
};

template<typename T>
struct unsized_allocator
{
  typedef T value_type;
};


void test01()
{
  typedef std::allocator_traits<maxsize_allocator<X>> traits_type;
  traits_type::allocator_type a;
  auto size = a.max_size();
  VERIFY( traits_type::max_size(a) == size );
}

void test02()
{
  typedef std::allocator_traits<unsized_allocator<X>> traits_type;
  traits_type::allocator_type a;
  auto size = std::numeric_limits<traits_type::size_type>::max();
  VERIFY( traits_type::max_size(a) == size / sizeof(X) );
}

void test03()
{
  typedef std::allocator_traits<unsized_allocator<int>> traits_type;
  traits_type::allocator_type a;
  auto size = std::numeric_limits<traits_type::size_type>::max();
  VERIFY( traits_type::max_size(a) == size / sizeof(int) );
}

int main()
{
  test01();
  test02();
  test03();
}
