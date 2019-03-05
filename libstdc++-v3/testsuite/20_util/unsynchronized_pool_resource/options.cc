// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <memory_resource>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

bool eq(const std::pmr::pool_options& lhs, const std::pmr::pool_options& rhs)
{
  return lhs.max_blocks_per_chunk == rhs.max_blocks_per_chunk
    && lhs.largest_required_pool_block == rhs.largest_required_pool_block;
}

void
test01()
{
  std::pmr::unsynchronized_pool_resource r0;
  const std::pmr::pool_options opts = r0.options();
  VERIFY( opts.max_blocks_per_chunk != 0 );
  VERIFY( opts.largest_required_pool_block != 0 );

  std::pmr::unsynchronized_pool_resource r1(opts);
  const auto opts1 = r1.options();
  VERIFY( eq(opts, opts1) );

  std::pmr::unsynchronized_pool_resource r2(std::pmr::pool_options{0, 0});
  const auto opts2 = r2.options();
  VERIFY( eq(opts, opts2) );
}

void
test02()
{
  std::pmr::pool_options opts{0, 0};
  std::size_t num_allocs = 0;

  __gnu_test::memory_resource test_mr;

  std::pmr::unsynchronized_pool_resource r1(opts, &test_mr);
  opts = r1.options();
  // opts.largest_required_pool_block should be set to the block size of
  // the largest pool (this is a GNU extension). Confirm this by checking
  // that allocations larger than opts.largest_required_pool_block come
  // directly from the upstream allocator, test_mr, not from r1's pools.

  // The following should result in a "large" allocation direct from upstream:
  (void) r1.allocate(opts.largest_required_pool_block + 1);
  num_allocs = test_mr.number_of_active_allocations();
  // This should result in another "large" allocation direct from upstream:
  (void) r1.allocate(opts.largest_required_pool_block + 1);
  // Which means the number of upstream allocations should have increased:
  VERIFY( test_mr.number_of_active_allocations() > num_allocs );
  r1.release();

  // Repeat with a user-specified block size:
  opts.largest_required_pool_block = 64;
  std::pmr::unsynchronized_pool_resource r2(opts, &test_mr);
  opts = r2.options();
  (void) r2.allocate(opts.largest_required_pool_block + 1);
  num_allocs = test_mr.number_of_active_allocations();
  (void) r2.allocate(opts.largest_required_pool_block + 1);
  VERIFY( test_mr.number_of_active_allocations() > num_allocs );
  r2.release();

  // Repeat with an odd user-specified block size:
  opts.largest_required_pool_block = 71;
  std::pmr::unsynchronized_pool_resource r3(opts, &test_mr);
  opts = r3.options();
  (void) r3.allocate(opts.largest_required_pool_block + 1);
  num_allocs = test_mr.number_of_active_allocations();
  (void) r3.allocate(opts.largest_required_pool_block + 1);
  VERIFY( test_mr.number_of_active_allocations() > num_allocs );
  r3.release();
}

int
main()
{
  test01();
  test02();
}
