// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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
#include <vector>
#include <tuple>
#include "../../../../include/std/pmroptional"

struct value_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  value_type(std::allocator_arg_t,allocator_type)
  : value_type(){}


  value_type(std::allocator_arg_t,allocator_type, int i)
  : value_type(i){}

  value_type(std::allocator_arg_t,allocator_type, int i, int j)
    : value_type(i, j){}

  value_type(std::allocator_arg_t,allocator_type, std::initializer_list<int> il)
      : value_type(il){}


  value_type(std::allocator_arg_t,allocator_type, value_type const& other)
    : value_type(other){}

  value_type(){};

  value_type(int _i) : i(_i){};

  value_type(int _i, int _j) : i(_i+_j){};

  value_type(std::initializer_list<int>) : i(1234){};

  value_type(value_type const& other): i(other.i)
      {};

  value_type& operator=(value_type const& other)
  {
    i = other.i;
    return *this;
  }
  int i = 55;
};
int main()
{
  value_type v = 42;
  auto o = std::pmr::make_optional(v);
  static_assert( std::is_same<decltype(o), std::pmr::optional<value_type>>(), "" );
  VERIFY( o && o->i == 42 );
  VERIFY( &*o != &v );

  auto o2 = std::pmr::make_optional<value_type>(1,3);
  static_assert( std::is_same<decltype(o2), std::pmr::optional<value_type>>(), "" );
  VERIFY( o2 && o2->i == 4 );


  auto o3 = std::pmr::make_optional<value_type>();
  static_assert( std::is_same<decltype(o3), std::pmr::optional<value_type>>(), "" );
  VERIFY( o3 && o3->i == 55 );

  auto o4 = std::pmr::make_optional<value_type>({1,3});
  static_assert( std::is_same<decltype(o4), std::pmr::optional<value_type>>(), "" );
  VERIFY( o4 && o4->i == 1234 );

}
