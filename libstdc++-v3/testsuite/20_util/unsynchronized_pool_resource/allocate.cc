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

#include <memory_resource>
#include <cstring>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_test::memory_resource test_mr;
  {
    std::pmr::unsynchronized_pool_resource r(&test_mr);
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
  std::pmr::unsynchronized_pool_resource r(&test_mr);
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
    std::pmr::unsynchronized_pool_resource r({10, 16}, &test_mr);
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
    std::pmr::unsynchronized_pool_resource r({16, 16}, &test_mr);
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
  std::pmr::unsynchronized_pool_resource r({256, 256}, &test_mr);
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

void
test05()
{
  __gnu_test::memory_resource test_mr;
  std::pmr::pool_options opts{};
  opts.max_blocks_per_chunk = 1;
  opts.largest_required_pool_block = 1;
  std::pmr::unsynchronized_pool_resource r(opts, &test_mr);
  opts = r.options();
  // Test unpooled allocations
  void** p = new void*[opts.largest_required_pool_block];
  for (unsigned a : {64, 128, 256, 512})
  {
    for (unsigned i = 0; i < opts.largest_required_pool_block; ++i)
      p[i] = r.allocate(i, a);
    for (unsigned i = 0; i < opts.largest_required_pool_block; ++i)
      r.deallocate(p[i], i, a);
  }
  delete[] p;
}

void
test06()
{
  struct checking_mr : std::pmr::memory_resource
  {
    size_t expected_size = 0;
    size_t expected_alignment = 0;

    struct bad_size { };
    struct bad_alignment { };

    void* do_allocate(std::size_t bytes, std::size_t align)
    {
      // Internal data structures in unsynchronized_pool_resource need to
      // allocate memory, so handle those normally:
      if (align <= alignof(std::max_align_t))
	return std::pmr::new_delete_resource()->allocate(bytes, align);

      // This is a large, unpooled allocation. Check the arguments:
      if (bytes < expected_size)
	throw bad_size();
      else if (align != expected_alignment)
      {
	if (bytes == std::numeric_limits<std::size_t>::max()
	    && align == (1 + std::numeric_limits<std::size_t>::max() / 2))
	{
	  // Pool resources request bytes=SIZE_MAX && align=bit_floor(SIZE_MAX)
	  // when they are unable to meet an allocation request.
	}
	else
	  throw bad_alignment();
      }
      // Else just throw, don't really try to allocate:
      throw std::bad_alloc();
    }

    void do_deallocate(void* p, std::size_t bytes, std::size_t align)
    { std::pmr::new_delete_resource()->deallocate(p, bytes, align); }

    bool do_is_equal(const memory_resource&) const noexcept
    { return false; }
  };

  checking_mr c;
  std::pmr::unsynchronized_pool_resource r({1, 1}, &c);
  std::pmr::pool_options opts = r.options();
  const std::size_t largest_pool = opts.largest_required_pool_block;
  const std::size_t large_alignment = 1024;
  // Ensure allocations won't fit in pools:
  VERIFY( largest_pool < large_alignment );

  // Ensure the vector of large allocations has some capacity
  // and won't need to reallocate:
  r.deallocate(r.allocate(largest_pool + 1, 1), largest_pool + 1, 1);

  // Try allocating various very large sizes and ensure the size requested
  // from the upstream allocator is at least as large as needed.
  for (int i = 0; i < std::numeric_limits<std::size_t>::digits; ++i)
  {
    for (auto b : { -63, -5, -1, 0, 1, 3, std::numeric_limits<int>::max() })
    {
      std::size_t bytes = std::size_t(1) << i;
      bytes += b; // For negative b this can wrap to a large positive value.
      c.expected_size = bytes;
      c.expected_alignment = large_alignment;
      bool caught_bad_alloc = false;
      try {
	(void) r.allocate(bytes, large_alignment);
      } catch (const std::bad_alloc&) {
	// expect to catch bad_alloc
	caught_bad_alloc = true;
      } catch (checking_mr::bad_size) {
	VERIFY( ! "allocation from upstream resource had expected size" );
      } catch (checking_mr::bad_alignment) {
	VERIFY( ! "allocation from upstream resource had expected alignment" );
      }
      VERIFY( caught_bad_alloc );
    }
  }
}

void
test07()
{
  // Custom exception thrown on expected allocation failure.
  struct very_bad_alloc : std::bad_alloc { };

  struct careful_resource : __gnu_test::memory_resource
  {
    void* do_allocate(std::size_t bytes, std::size_t alignment)
    {
      // Need to allow normal allocations for the pool resource's internal
      // data structures:
      if (alignment < 1024)
	return __gnu_test::memory_resource::do_allocate(bytes, alignment);

      // pmr::unsynchronized_pool_resource::do_allocate is not allowed to
      // throw an exception when asked for an allocation it can't satisfy.
      // The libstdc++ implementation will ask upstream to allocate
      // bytes=SIZE_MAX and alignment=bit_floor(SIZE_MAX) instead of throwing.
      // Verify that we got those values:
      if (bytes != std::numeric_limits<size_t>::max())
	VERIFY( !"upstream allocation should request SIZE_MAX bytes" );
      if (alignment != (1 + std::numeric_limits<size_t>::max() / 2))
	VERIFY( !"upstream allocation should request SIZE_MAX/2 alignment" );

      // A successful failure:
      throw very_bad_alloc();
    }
  };

  careful_resource cr;
  std::pmr::unsynchronized_pool_resource upr(&cr);
  try
  {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Walloc-size-larger-than="
    // Try to allocate a ridiculous size (and use a large extended alignment
    // so that careful_resource::do_allocate can distinguish this allocation
    // from any required for the pool resource's internal data structures):
    void* p = upr.allocate(std::size_t(-2), 1024);
#pragma GCC diagnostic pop
    // Should not reach here!
    VERIFY( !"attempt to allocate SIZE_MAX-1 should not have succeeded" );
    throw p;
  }
  catch (const very_bad_alloc&)
  {
    // Should catch this exception from careful_resource::do_allocate
  }
  catch (const std::bad_alloc&)
  {
    VERIFY( !"unsynchronized_pool_resource::do_allocate is not allowed to throw" );
  }
}

void
test08()
{
  std::pmr::pool_options opts;
  opts.largest_required_pool_block = 64;

  // PR libstdc++/94160
  // max_blocks_per_chunk=1 causes pool resources to return null pointers
  for (int i = 0; i < 8; ++i)
  {
    opts.max_blocks_per_chunk = i;
    std::pmr::unsynchronized_pool_resource upr(opts);
    auto* p = (int*)upr.allocate(4);
    VERIFY( p != nullptr );
    *p = i;
    upr.deallocate(p, 4);
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
}
