// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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
#include <cstring>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_test::memory_resource test_mr;
  {
    std::pmr::synchronized_pool_resource r(&test_mr);
    void* p1 = r.allocate(1, 1);
    VERIFY( p1 != nullptr );
    auto n = test_mr.number_of_active_allocations();
    VERIFY( n > 0 );
    // Ensure memory region can be written to (without corrupting heap!)
    std::memset(p1, 0xff, 1);
    void* p2 = r.allocate(1, 1);
    VERIFY( p2 != nullptr );
    VERIFY( p2 != p1 );
    VERIFY( test_mr.number_of_active_allocations() == n );
    std::memset(p1, 0xff, 1);
    r.deallocate(p1, 1, 1);
    // Returning single blocks to the pool doesn't return them upstream:
    VERIFY( test_mr.number_of_active_allocations() == n );
    r.deallocate(p2, 1, 1);
    VERIFY( test_mr.number_of_active_allocations() == n );
  }
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
  void* p1 = r.allocate(8, 1);
  VERIFY( p1 != nullptr );
  std::memset(p1, 0xff, 8);
  test_mr.upstream = nullptr;
  void* p2 = r.allocate(8, 1); //should not need to replenish
  VERIFY( p2 != nullptr );
  VERIFY( p2 != p1 );
  std::memset(p1, 0xff, 8);
  r.deallocate(p1, 8, 1); // should not use upstream
  r.deallocate(p2, 8, 1); // should not use upstream

  // Destructor will return memory upstream, so restore the upstream resource:
  test_mr.upstream = std::pmr::get_default_resource();
}

void
test03()
{
  __gnu_test::memory_resource test_mr;
  {
    std::pmr::synchronized_pool_resource r({10, 16}, &test_mr);
    std::size_t largest_pool = r.options().largest_required_pool_block;
    void* p1 = r.allocate(2 * largest_pool);
    VERIFY( p1 != nullptr );
    const std::size_t n = test_mr.number_of_active_allocations();
    // Allocation of pools + allocation of pmr::vector + oversize allocation:
    VERIFY( n >= 1 );
    std::memset(p1, 0xff, 2 * largest_pool);
    void* p2 = r.allocate(3 * largest_pool);
    VERIFY( p2 != nullptr );
    VERIFY( p2 != p1 );
    VERIFY( test_mr.number_of_active_allocations() == n + 1 );
    std::memset(p2, 0xff, 3 * largest_pool);
    r.deallocate(p1, 2 * largest_pool);
    VERIFY( test_mr.number_of_active_allocations() ==  n );
    r.deallocate(p2, 3 * largest_pool);
    VERIFY( test_mr.number_of_active_allocations() == n - 1 );
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

void
test04()
{
  __gnu_test::memory_resource test_mr;
  std::pmr::synchronized_pool_resource r({256, 256}, &test_mr);
  // Check alignment
  void* p1 = r.allocate(2, 64);
  VERIFY( (std::uintptr_t)p1 % 64 == 0 );
  void* p2 = r.allocate(2, 128);
  VERIFY( (std::uintptr_t)p2 % 128 == 0 );
  void* p3 = r.allocate(2, 256);
  VERIFY( (std::uintptr_t)p3 % 256 == 0 );
  const std::size_t largest_pool = r.options().largest_required_pool_block;
  void* p4 = r.allocate(2 * largest_pool, 1024);
  VERIFY( (std::uintptr_t)p4 % 1024 == 0 );
  r.deallocate(p1, 2, 64);
  r.deallocate(p2, 2, 128);
  r.deallocate(p3, 2, 256);
  r.deallocate(p4, 2 * largest_pool, 1024);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
