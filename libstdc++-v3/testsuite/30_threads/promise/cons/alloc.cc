// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

using std::promise;
using std::allocator_arg;

void test01()
{
  __gnu_test::uneq_allocator<char> alloc(99);
  promise<int> p1(allocator_arg, alloc);
  p1.set_value(5);
  VERIFY( p1.get_future().get() == 5 );
}

void test02()
{
  __gnu_test::CustomPointerAlloc<int> alloc;
  promise<int> p1(allocator_arg, alloc);
  p1.set_value(5);
  VERIFY( p1.get_future().get() == 5 );
}

int main()
{
  test01();
  test02();
  return 0;
}
