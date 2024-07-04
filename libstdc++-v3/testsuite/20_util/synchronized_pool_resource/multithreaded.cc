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
#include <future>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_test::memory_resource test_mr;
  std::pmr::synchronized_pool_resource smr(&test_mr);

  const std::size_t largest_pool = smr.options().largest_required_pool_block;

  auto do_alloc = [&smr](void*& p, size_t n) {
    // perform some other allocations and deallocations on the same thread:
    void* p2 = smr.allocate(n);
    smr.deallocate(p2, n);
    p2 = smr.allocate(n);
    p = smr.allocate(n);
    smr.deallocate(p2, n);
  };
  auto do_dealloc = [&smr](void* p, size_t n) { smr.deallocate(p, n); };

  void* p1;
  void* p2;
  void* p3;
  auto f1 = std::async(std::launch::async, do_alloc, std::ref(p1), 8);
  auto f2 = std::async(std::launch::async, do_alloc, std::ref(p2), 64);
  auto f3 = std::async(std::launch::async, do_alloc, std::ref(p3),
		       largest_pool* 2);

  f1.get();
  f2.get();
  f3.get();
  VERIFY( p1 != nullptr );
  VERIFY( p2 != nullptr );
  VERIFY( p3 != nullptr );
  size_t nallocs = test_mr.number_of_active_allocations();
  VERIFY( nallocs >= 4 );

  // deallocate on different threads from allocation:
  f1 = std::async(std::launch::async, do_dealloc, p1, 8);
  f2 = std::async(std::launch::async, do_dealloc, p2, 64);
  f1.get();
  f2.get();
  // No additional memory is allocated by deallocating on new threads:
  VERIFY( test_mr.number_of_active_allocations() == nallocs );

  // Deallocate large unpooled allocation:
  f3 = std::async(std::launch::async, do_dealloc, p3, largest_pool * 2);
  f3.get();
  // The large allocation should have been returned upstream:
  VERIFY( test_mr.number_of_active_allocations() == nallocs - 1 );

  smr.release();
  VERIFY( test_mr.number_of_active_allocations() == 0 );
}

int
main()
{
  test01();
}
