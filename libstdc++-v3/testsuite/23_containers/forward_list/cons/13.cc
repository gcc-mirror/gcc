// { dg-options "-std=gnu++11" }

// Copyright (C) 2012-2013 Free Software Foundation, Inc.
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
#include <memory>
#include <scoped_allocator>
#include <testsuite_hooks.h>

struct A
{
  typedef std::allocator<A> allocator_type;

  A() : ok(false) { }
  A(const A&) : ok(false) { }
  A(const allocator_type&) : ok(true) { }
  A(const A&, const allocator_type&) : ok(true) { }

  bool ok;
};

void test01()
{
  typedef std::scoped_allocator_adaptor<A::allocator_type> alloc_type;
  typedef std::forward_list<A, alloc_type> list;

  list l1(1);
  VERIFY( l1.begin()->ok );

  A a;
  list l2(1, a);
  VERIFY( l2.begin()->ok );
}

int main()
{
  test01();
}
