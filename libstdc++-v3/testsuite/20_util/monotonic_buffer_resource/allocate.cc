// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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
// { dg-require-cstdint "" }

#include <memory_resource>
#include <testsuite_allocator.h>

void
test01()
{
  __gnu_test::memory_resource r;

  // test that it's possible to allocate after each of the constructors
  {
    std::pmr::monotonic_buffer_resource mr(&r);
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
  }
  VERIFY( r.number_of_active_allocations() == 0 );
  {
    std::pmr::monotonic_buffer_resource mr(128, &r);
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
  }
  VERIFY( r.number_of_active_allocations() == 0 );
  {
    unsigned char buf[64];
    std::pmr::monotonic_buffer_resource mr((void*)buf, sizeof(buf), &r);
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
  }
  VERIFY( r.number_of_active_allocations() == 0 );
  {
    std::pmr::monotonic_buffer_resource mr;
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
  }
  {
    std::pmr::monotonic_buffer_resource mr(64);
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
  }
  {
    unsigned char buf[64];
    std::pmr::monotonic_buffer_resource mr((void*)buf, sizeof(buf));
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
  }
}

void
test02()
{
  unsigned char buf[64];
  std::pmr::monotonic_buffer_resource mr(buf, sizeof(buf));

  auto p = mr.allocate(0);
  VERIFY( p != nullptr );
  auto q = mr.allocate(0);
  VERIFY( q != nullptr );
  VERIFY( p != q );

  p = mr.allocate(0, 1);
  VERIFY( p != nullptr );
  q = mr.allocate(0, 1);
  VERIFY( q != nullptr );
  VERIFY( p != q );
}

void
test03()
{
#if __cpp_exceptions
  {
    std::pmr::monotonic_buffer_resource mr(std::pmr::null_memory_resource());
    bool caught = false;
    try
    {
      (void) mr.allocate(1, 1);
    }
    catch (const std::bad_alloc&)
    {
      caught = true;
    }
    VERIFY( caught );
  }
  {
    unsigned char buf[16];
    std::pmr::monotonic_buffer_resource mr(buf, sizeof(buf),
					   std::pmr::null_memory_resource());
    (void) mr.allocate(16, 1);
    bool caught = false;
    try
    {
      (void) mr.allocate(1, 1);
    }
    catch (const std::bad_alloc&)
    {
      caught = true;
    }
    VERIFY( caught );
  }
#endif
}

void
test04()
{
  auto buf = new unsigned char[512];
  std::pmr::monotonic_buffer_resource mr(buf, 512,
					 std::pmr::null_memory_resource());
  std::size_t prev_size = 1;
  void* prev_ptr = mr.allocate(prev_size, 1);
  for (int i = 0; i < 9; ++i)
  {
    std::size_t size = 1 << i;
    void* ptr = mr.allocate(size, 1);
    VERIFY( ((char*)ptr - (char*)prev_ptr) == prev_size );
    prev_ptr = ptr;
    prev_size = size;
  }
}

void
test05()
{
  // test that returned pointer is correctly aligned
  auto is_aligned = [](void* p, size_t alignment) -> bool {
    return (reinterpret_cast<std::uintptr_t>(p) % alignment) == 0;
  };

  auto buf = new unsigned char[2048];
  std::pmr::monotonic_buffer_resource mr(buf+1, 2047);
  for (int i = 0; i < 9; ++i)
  {
    auto p = mr.allocate(1, 1 << i);
    VERIFY( is_aligned(p, 1 << i) );
    // Make next available byte misaligned:
    (void) mr.allocate(1 << i, 1);
  }
}

void
test06()
{
  // check for geometric progression in buffer sizes from upstream

  struct resource : __gnu_test::memory_resource
  {
    bool allocated = false;
    std::size_t last_size = 0;

    void*
    do_allocate(size_t bytes, size_t align) override
    {
      allocated = true;
      last_size = bytes;
      return __gnu_test::memory_resource::do_allocate(bytes, align);
    }
  };

  resource r;
  std::pmr::monotonic_buffer_resource mr(32, &r);
  std::size_t last_size = 0;

  for (int i = 0; i < 100; ++i)
  {
    (void) mr.allocate(16);
    if (r.allocated)
    {
      VERIFY(r.last_size >= last_size);
      last_size = r.last_size;
      r.allocated = false;
    }
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
}
