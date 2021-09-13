// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <memory>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

void test_make_obj_using_allocator()
{
  std::allocator<aggressive_aggregate> a;
  auto x =
    std::make_obj_using_allocator<aggressive_aggregate>(a, 1, 2);
  VERIFY(x.a == 1);
  VERIFY(x.b == 2);
  x = std::make_obj_using_allocator<aggressive_aggregate>(a, 1);
  VERIFY(x.a == 1);
  VERIFY(x.b == 0);
  x = std::make_obj_using_allocator<aggressive_aggregate>(a);
  VERIFY(x.a == 0);
  VERIFY(x.b == 0);
}

void test_uninitialized_construct_using_allocator()
{
  std::allocator<aggressive_aggregate> a;
  aggressive_aggregate x;
  std::destroy_at(&x);
  std::uninitialized_construct_using_allocator(&x, a, 1, 2);
  VERIFY(x.a == 1);
  VERIFY(x.b == 2);
  std::destroy_at(&x);
  std::uninitialized_construct_using_allocator(&x, a, 1);
  VERIFY(x.a == 1);
  VERIFY(x.b == 0);
  std::destroy_at(&x);
  std::uninitialized_construct_using_allocator(&x, a);
  VERIFY(x.a == 0);
  VERIFY(x.b == 0);
}

int main()
{
  test_make_obj_using_allocator();
  test_uninitialized_construct_using_allocator();
}
