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
  // [20.5.5] In-place construction
  {
    std::pmr::optional<value_type> o { std::in_place };
    VERIFY( o );
    VERIFY( o->i == 55 );

    static_assert( !std::is_convertible<std::in_place_t, std::pmr::optional<value_type>>(), "" );
  }

  {
    std::pmr::optional<value_type> o { std::in_place, 42 };
    VERIFY( o );
    VERIFY( o->i == 42 );
  }

  {
    std::pmr::optional<value_type> o { std::in_place, 18, 4 };
    VERIFY( o );
    VERIFY( o->i == 22 );
  }

  {
    std::pmr::optional<value_type> o { std::in_place, { 18, 4 } };
    VERIFY( o );
    VERIFY( o->i == 1234 );
  }

}
