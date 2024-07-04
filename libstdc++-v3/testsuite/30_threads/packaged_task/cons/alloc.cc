// { dg-options "-std=gnu++11" }
// { dg-do run { target { c++11_only || c++14_only } } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

#include <future>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

int f() { return 5; }

void test01()
{
  using std::packaged_task;
  using std::allocator_arg;
  using __gnu_test::uneq_allocator;

  uneq_allocator<char> alloc(99);

  packaged_task<int ()> p1(allocator_arg, alloc, f);
  VERIFY( p1.valid() );
  p1();
  VERIFY( p1.get_future().get() == 5 );
}

int main()
{
  test01();
  return 0;
}
