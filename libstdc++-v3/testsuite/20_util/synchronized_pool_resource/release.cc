// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

#include <memory_resource>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_test::memory_resource test_mr;
  std::pmr::synchronized_pool_resource r(&test_mr);
  r.release();
  VERIFY( test_mr.number_of_active_allocations() == 0 );
  r.release();
  VERIFY( test_mr.number_of_active_allocations() == 0 );
  (void) r.allocate(1);
  VERIFY( test_mr.number_of_active_allocations() != 0 );
  r.release();
  VERIFY( test_mr.number_of_active_allocations() == 0 );
  r.release();
  VERIFY( test_mr.number_of_active_allocations() == 0 );
}

void
test02()
{
  struct nullable_memory_resource : public std::pmr::memory_resource
  {
    void*
    do_allocate(std::size_t bytes, std::size_t alignment) override
    { return upstream->allocate(bytes, alignment); }

    void
    do_deallocate(void* p, std::size_t bytes, std::size_t alignment) override
    { upstream->deallocate(p, bytes, alignment); }

    bool
    do_is_equal(const memory_resource& r) const noexcept override
    { return &r == this; }

    std::pmr::memory_resource* upstream = std::pmr::get_default_resource();
  };

  nullable_memory_resource test_mr;
  std::pmr::synchronized_pool_resource r(&test_mr);
  r.release();
  test_mr.upstream = nullptr;
  r.release(); // should not need to call anything through upstream pointer
}

void
test03()
{
  __gnu_test::memory_resource test_mr;
  {
    std::pmr::synchronized_pool_resource r(&test_mr);
    // Destructor calls release()
  }
  VERIFY( test_mr.number_of_active_allocations() == 0 );
  {
    std::pmr::synchronized_pool_resource r(&test_mr);
    (void) r.allocate(1);
    VERIFY( test_mr.number_of_active_allocations() != 0 );
    // Destructor calls release()
  }
  VERIFY( test_mr.number_of_active_allocations() == 0 );
  {
    std::pmr::synchronized_pool_resource r({10, 16}, &test_mr);
    (void) r.allocate(2 * r.options().largest_required_pool_block);
    VERIFY( test_mr.number_of_active_allocations() != 0 );
    // Destructor calls release()
  }
  VERIFY( test_mr.number_of_active_allocations() == 0 );
  {
    std::pmr::synchronized_pool_resource r({16, 16}, &test_mr);
    (void) r.allocate(2);
    (void) r.allocate(8);
    (void) r.allocate(16);
    (void) r.allocate(2);
    (void) r.allocate(8);
    (void) r.allocate(16);
    (void) r.allocate(2 * r.options().largest_required_pool_block);
    VERIFY( test_mr.number_of_active_allocations() != 0 );
    // Destructor calls release()
  }
  VERIFY( test_mr.number_of_active_allocations() == 0 );
}

int
main()
{
  test01();
  test02();
  test03();
}
