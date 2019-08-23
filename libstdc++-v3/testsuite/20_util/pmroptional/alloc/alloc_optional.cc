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

bool tagged_constructor_called = false;
bool plain_constructor_called = false;

void reset_flags()
{
  tagged_constructor_called = false;
  plain_constructor_called = false;
}
struct my_pmr : public std::pmr::memory_resource {

	virtual void* do_allocate(size_t bytes, size_t alignment) { return nullptr; };
	virtual void do_deallocate(void* p, size_t bytes, size_t alignment){};
	virtual bool do_is_equal(const std::pmr::memory_resource& other) const noexcept { return true;};


} test_pmr;

struct value_type
{
  static my_pmr*  domain_alloc_convert(std::pmr::polymorphic_allocator<void> _a) { return &test_pmr; };
  value_type(my_pmr* alloc)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_pmr);
  }


  value_type(int _i,my_pmr* alloc)
  : i(_i)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_pmr);
  }


  value_type(value_type const& other, my_pmr* alloc)
  : i(other.i)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_pmr);
  }

  value_type(int i, int j, my_pmr* alloc)
  : i(i+j)
  {
    tagged_constructor_called = true;
    VERIFY( alloc == &test_pmr);
  }

  value_type(std::initializer_list<int> il,my_pmr* alloc)
  : i(1234)
  {
	tagged_constructor_called = true;
	VERIFY( alloc == &test_pmr);
  }


  value_type(){};

  value_type(int _i) : i(_i){ plain_constructor_called = true;}

  value_type(int _i, int _j) : i(_i+_j){ plain_constructor_called = true;};

  value_type(std::initializer_list<int>) : i(1234){ plain_constructor_called = true;};


  value_type(value_type const& other): i(other.i)
  { plain_constructor_called = true;}

  value_type& operator=(value_type const& other)
  {
    i = other.i;
    return *this;
  }
  int i = 55;
};
int main()
{
  std::pmr::polymorphic_allocator<void> my_alloc(&test_pmr);
  static_assert(std::uses_allocator<value_type, std::pmr::polymorphic_allocator<void>>{}, "");
  value_type v = 42;
  reset_flags();

  auto o = std::pmr::alloc_optional<value_type>(my_alloc, v);
  static_assert( std::is_same<decltype(o), std::pmr::optional<value_type>>(), "" );
  VERIFY( o && o->i == 42 );
  VERIFY( &*o != &v );
  VERIFY( tagged_constructor_called);
  VERIFY( !plain_constructor_called);
  reset_flags();

  auto o2 = std::pmr::alloc_optional<value_type>(my_alloc, 1,3);
  static_assert( std::is_same<decltype(o2), std::pmr::optional<value_type>>(), "" );
  VERIFY( o2 && o2->i == 4 );
  VERIFY( tagged_constructor_called);
  VERIFY( !plain_constructor_called);
  reset_flags();


  auto o3 = std::pmr::alloc_optional<value_type>(my_alloc);
  static_assert( std::is_same<decltype(o3), std::pmr::optional<value_type>>(), "" );
  VERIFY( o3 && o3->i == 55 );
  VERIFY( tagged_constructor_called);
  VERIFY( !plain_constructor_called);
  reset_flags();

  auto o4 = std::pmr::alloc_optional<value_type>(my_alloc,{1,3});
  static_assert( std::is_same<decltype(o4), std::pmr::optional<value_type>>(), "" );
  VERIFY( o4 && o4->i == 1234 );
  VERIFY( tagged_constructor_called);
  VERIFY( !plain_constructor_called);
  reset_flags();

}
