// { dg-do run { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

struct X { };

template<typename T>
struct alloc1
{
  typedef T value_type;

  int id;
};

template<typename T>
struct alloc2
{
  typedef T value_type;

  int id;

  alloc2 select_on_container_copy_construction() const
  { return alloc2{id+1}; }
};


void test01()
{
  typedef std::allocator_traits<alloc1<X>> traits_type;
  traits_type::allocator_type a{1};
  const traits_type::allocator_type& a2
    = traits_type::select_on_container_copy_construction(a);
  VERIFY( a2.id == a.id );
}

void test02()
{
  typedef std::allocator_traits<alloc2<X>> traits_type;
  traits_type::allocator_type a{1};
  const traits_type::allocator_type& a2
    = traits_type::select_on_container_copy_construction(a);
  VERIFY( a2.id != a.id );
}

int main()
{
  test01();
  test02();
}
