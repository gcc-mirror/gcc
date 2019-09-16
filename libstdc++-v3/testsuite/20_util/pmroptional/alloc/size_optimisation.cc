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


};

struct value_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;
  value_type(std::allocator_arg_t,allocator_type alloc)
  {
    tagged_constructor_called = true;
    M_pmr = (my_pmr *)alloc.resource();
  }


  value_type(std::allocator_arg_t,allocator_type alloc, int _i)
  : i(_i)
  {
    tagged_constructor_called = true;
    M_pmr = (my_pmr *)alloc.resource();
  }


  value_type(std::allocator_arg_t,allocator_type alloc, value_type const& other)
  : i(other.i)
  {
    tagged_constructor_called = true;
    M_pmr = (my_pmr *)alloc.resource();
  }

  value_type(std::allocator_arg_t,allocator_type alloc, int i, int j)
  : i(i+j)
  {
    tagged_constructor_called = true;
    M_pmr = (my_pmr *)alloc.resource();
  }

  value_type(std::allocator_arg_t,allocator_type alloc, std::initializer_list<int> il)
  : i(1234)
  {
	tagged_constructor_called = true;
    M_pmr = (my_pmr *)alloc.resource();
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

  allocator_type get_allocator() const
  {
	  return std::pmr::polymorphic_allocator<void>(M_pmr);
  }
  my_pmr *M_pmr;
};
//static_assert(std::is_convertible<decltype(std::declval<value_type&>().get_allocator()),std::pmr::polymorphic_allocator<void>>{});
static_assert(sizeof(std::pmr::optional<value_type>) == sizeof(std::optional<value_type>));
int main()
{
  my_pmr test_pmr1, test_pmr2;
  std::pmr::polymorphic_allocator<void> my_alloc1(&test_pmr1);
  std::pmr::polymorphic_allocator<void> my_alloc2(&test_pmr2);

  {
	  reset_flags();
	  auto o1 = std::pmr::optional<value_type>(std::allocator_arg, my_alloc1);
	  VERIFY(o1.get_allocator().resource() == &test_pmr1);
	  VERIFY( !o1);

	  value_type v(std::allocator_arg, my_alloc2, 42);

	  o1 = v;

	  VERIFY(o1.get_allocator().resource() == &test_pmr1);
	  VERIFY( o1 && o1->i == 42);

	  o1.reset();

	  VERIFY(o1.get_allocator().resource()== &test_pmr1);
	  VERIFY( !o1);



	  auto o2 = std::pmr::optional<value_type>(std::allocator_arg, my_alloc2);
	  VERIFY(o2.get_allocator().resource()==&test_pmr2);

	  o1 = o2;
	  VERIFY(o1.get_allocator().resource()== &test_pmr1);
	  VERIFY( !o1);

	  o2 = 3;

	  VERIFY(o2.get_allocator().resource()== &test_pmr2);
	  VERIFY(o2 && o2->i == 3);

	  o1 = std::move(o2);
	  VERIFY(o1.get_allocator().resource()== &test_pmr1);
	  VERIFY(o1 && o1->i == 3);

	  VERIFY( tagged_constructor_called);
	  VERIFY( !plain_constructor_called);
	  reset_flags();

  }

}
