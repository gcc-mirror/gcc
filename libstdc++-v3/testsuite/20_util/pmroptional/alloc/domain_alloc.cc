// { dg-options "-std=gnu++17" }
// { dg-do run }

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

#include <any>
#include "../../../../include/std/pmroptional"

using std::pmr::optional;

#include <testsuite_hooks.h>

bool tagged_constructor_called = false;
bool plain_constructor_called = false;

void reset_flags()
{
  tagged_constructor_called = false;
  plain_constructor_called = false;
}

struct my_alloc{} test_alloc;

struct value_type
{
  constexpr static my_alloc*  domain_alloc_rebind(std::pmr::polymorphic_allocator<void>const& _a) { return &test_alloc; };
  value_type(my_alloc* alloc)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_alloc);
  }


  value_type(int _i,my_alloc* alloc)
  : i(_i)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_alloc);
  }


  value_type(value_type const& other, my_alloc* alloc)
  : i(other.i)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_alloc);
  }

  value_type(){};

  value_type(int _i) : i(_i){ plain_constructor_called = true;}

  value_type(value_type const& other): i(other.i)
  { plain_constructor_called = true;}

  value_type& operator=(value_type const& other)
  {
    i = other.i;
    return *this;
  }
  int i = 0;
};

int main()
{
  reset_flags();
   {
     std::pmr::optional<value_type> ov;
     VERIFY( !tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();


     ov.emplace(2);
     VERIFY( tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();

   }

   {
     auto ov = std::pmr::make_optional<value_type>();
     VERIFY( tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();

   }

   {
     auto ov = std::pmr::make_optional<value_type>(33);
     reset_flags();

     std::vector<std::pmr::optional<value_type>> vov(18,4);
     VERIFY( tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();

     auto oa = ov;
     VERIFY( tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();

     auto ob = std::move(ov);
     VERIFY( tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();

   }

   {
     std::pmr::optional<value_type> ov{std::in_place, 1};
     VERIFY( tagged_constructor_called);
     VERIFY( !plain_constructor_called);
     reset_flags();
   }
}
