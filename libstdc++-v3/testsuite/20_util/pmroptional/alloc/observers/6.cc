// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include "../../../../../include/std/pmroptional"

struct Y
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  Y(std::allocator_arg_t,allocator_type)
  : Y(){}


  Y(std::allocator_arg_t,allocator_type, Y&& other)
  : Y(std::move(other)){}

  Y(std::allocator_arg_t,allocator_type, Y const& other)
    : Y(other){}

  Y() = default;
  int test() & {return 7;}
};

int
test()
{
  std::pmr::optional<Y> opt{Y{}};
  return opt.value().test();
}

int main()
{
  VERIFY( test() == 7 );
}
