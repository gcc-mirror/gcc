// { dg-do run { target c++14 } }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

#include <experimental/memory_resource>
#include <vector>
#include <cstdlib>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using std::experimental::pmr::polymorphic_allocator;
using std::experimental::pmr::memory_resource;
using std::experimental::pmr::new_delete_resource;
using std::experimental::pmr::get_default_resource;
using std::experimental::pmr::set_default_resource;

struct A
{
  A() { ++ctor_count; }
  ~A() { ++dtor_count; }
  static int ctor_count;
  static int dtor_count;
};

int A::ctor_count = 0;
int A::dtor_count = 0;

struct CountedResource : public memory_resource
{
public:
  CountedResource() = default;
  ~CountedResource() = default;

  static size_t get_alloc_count()  { return alloc_count;  }
  static size_t get_dalloc_count() { return dalloc_count; }

  static size_t  alloc_count;
  static size_t  dalloc_count;
protected:
  void* do_allocate(size_t bytes, size_t alignment)
  {
    alloc_count += bytes;
    if (auto ptr = std::malloc(bytes))
      return ptr;
    throw std::bad_alloc();
  }

  void do_deallocate(void *p, size_t bytes, size_t alignment)
  {
    dalloc_count += bytes;
    std::free(p);
  }

  bool do_is_equal(const memory_resource& __other) const noexcept
  { return this == &__other; }
};

size_t CountedResource::alloc_count  = 0;
size_t CountedResource::dalloc_count = 0;

void clear()
{
  CountedResource::alloc_count  = 0;
  CountedResource::dalloc_count = 0;
  A::ctor_count = 0;
  A::dtor_count = 0;
}

// memory resource
void
test01()
{
  memory_resource* r = new_delete_resource();
  VERIFY(get_default_resource() == r);
  void *p = get_default_resource()->allocate(5);
  VERIFY(p);
  get_default_resource()->deallocate(p, 5);

  clear();
  CountedResource* cr = new CountedResource();
  set_default_resource(cr);
  VERIFY(get_default_resource() == cr);
  void *pc = get_default_resource()->allocate(5);
  VERIFY(pc);
  get_default_resource()->deallocate(pc, 5);
  VERIFY(CountedResource::get_alloc_count()  == 5);
  VERIFY(CountedResource::get_dalloc_count() == 5);
}

// polymorphic_allocator
void
test02()
{
  clear();
  {
    CountedResource cr;
    polymorphic_allocator<A> pa(&cr);
    std::vector<A, polymorphic_allocator<A>> v(5, A(), pa);
  }
  VERIFY(A::ctor_count == 1);
  VERIFY(A::dtor_count == 6);
  VERIFY(CountedResource::get_alloc_count()  == 5);
  VERIFY(CountedResource::get_dalloc_count() == 5);
}

void
test03()
{
  clear();
  CountedResource cr;
  polymorphic_allocator<A> pa(&cr);
  A* p = pa.allocate(1);
  pa.construct(p);
  pa.destroy(p);
  pa.deallocate(p, 1);
  VERIFY(A::ctor_count == 1);
  VERIFY(A::dtor_count == 1);
  VERIFY(CountedResource::get_alloc_count()  == 1);
  VERIFY(CountedResource::get_dalloc_count() == 1);
}

void
test04()
{
  polymorphic_allocator<A> pa1(get_default_resource());
  polymorphic_allocator<A> pa2(get_default_resource());
  VERIFY(pa1 == pa2);
  polymorphic_allocator<A> pa3 = pa2.select_on_container_copy_construction();
  VERIFY(pa1 == pa3);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
