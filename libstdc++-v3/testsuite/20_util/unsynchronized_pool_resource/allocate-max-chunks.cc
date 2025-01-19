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

#include <memory_resource>
#include <testsuite_hooks.h>

struct custom_mr : std::pmr::memory_resource
{
  custom_mr(std::size_t max) : max(max) { }

  bool reached_max = false;

private:
  std::size_t max;
  std::size_t count = 0;

  void* do_allocate(std::size_t b, std::size_t a)
  {
    if (b >= max)
      reached_max = true;
    count += b;
    if (count > (18 * 1024 * 1024))
      // Something went wrong, should not need to allocate this much.
      throw std::bad_alloc();
    return std::pmr::new_delete_resource()->allocate(b, a);
  }

  void do_deallocate(void* p, std::size_t b, std::size_t a)
  { std::pmr::new_delete_resource()->deallocate(p, b, a); }

  bool do_is_equal(const memory_resource& r) const noexcept
  { return false; }
};

void
test01()
{
  // Only going to allocate blocks of this size:
  const std::size_t block_size = 8;
  std::pmr::pool_options opts{};
  // Use maximum allowed number of blocks per chunk:
  opts.max_blocks_per_chunk = (std::size_t)-1;
  opts.largest_required_pool_block = block_size;
  {
    std::pmr::unsynchronized_pool_resource r(opts);
    // Get the real max_blocks_per_chunk that will be used:
    opts = r.options();
    // Sanity test in case chunk::max_blocks_per_chunk() changes,
    // as that could make this test take much longer to run:
    VERIFY( opts.max_blocks_per_chunk <= (1 << 19) );
  }
  custom_mr c(block_size * opts.max_blocks_per_chunk);
  std::pmr::unsynchronized_pool_resource r(opts, &c);
  // Keep allocating from the pool until reaching the maximum chunk size:
  while (!c.reached_max)
    (void) r.allocate(block_size, 1);
  c.reached_max = false;
  // Now fill that maximally-sized chunk
  // (this used to go into an infinite loop ):
  for (std::size_t i = 0; i < opts.max_blocks_per_chunk; ++i)
    (void) r.allocate(block_size, 1);
  // Should have filled the maximally-sized chunk and allocated another
  // maximally-sized chunk from upstream:
  VERIFY( c.reached_max );
}

int
main()
{
  test01();
}
