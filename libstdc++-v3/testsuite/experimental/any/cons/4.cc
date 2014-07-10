// { dg-options "-std=gnu++14" }
// { dg-do run }

// Copyright (C) 2014 Free Software Foundation, Inc.
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

#include <experimental/any>
#include <memory>
#include <testsuite_hooks.h>

using std::experimental::any;

struct NotSmall
{
  char c[64];  // prevent small-object optimization
};

struct T1
{
  using allocator_type = std::allocator<char>;

  T1() = default;
  T1(const T1&) : used_alloc(false) { }
  T1(const T1&, const allocator_type&) : used_alloc(true) { }

  bool used_alloc;

  NotSmall x;
};

struct T2
{
  using allocator_type = std::allocator<char>;

  T2() = default;
  T2(const T2&) : used_alloc(false) { }
  T2(std::allocator_arg_t, const allocator_type&, const T2&) : used_alloc(true)
  { }

  bool used_alloc;

  NotSmall x;
};

bool test [[gnu::unused]] = true;

void test01()
{
  any x1(std::allocator_arg, std::allocator<char>{}, T1{});
  VERIFY( std::experimental::any_cast<T1&>(x1).used_alloc );

  any x2(std::allocator_arg, std::allocator<char>{}, T2{});
  VERIFY( std::experimental::any_cast<T2&>(x2).used_alloc );
}

int main()
{
  test01();
}
