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
#include <testsuite_allocator.h>

struct resource : __gnu_test::memory_resource
{
  int allocate_calls = 0;
  int deallocate_calls = 0;

  void*
  do_allocate(std::size_t bytes, std::size_t align) override
  {
    ++allocate_calls;
    return __gnu_test::memory_resource::do_allocate(bytes, align);
  }

  void
  do_deallocate(void* p, std::size_t bytes, std::size_t align) override
  {
    ++deallocate_calls;
    __gnu_test::memory_resource::do_deallocate(p, bytes, align);
  }
};

void
test01()
{
  resource r;
  std::pmr::monotonic_buffer_resource mbr(&r);
  auto p = mbr.allocate(10, 16);
  mbr.deallocate(p, 1, 2);
  VERIFY( r.deallocate_calls == 0 );
  p = mbr.allocate(10, 16);
  p = mbr.allocate(10, 16);
  p = mbr.allocate(10, 16);
  p = mbr.allocate(1024, 64);
  p = mbr.allocate(1024, 64);
  p = mbr.allocate(128, 8);
  p = mbr.allocate(128, 8);
  p = mbr.allocate(128, 8);
  p = mbr.allocate(128, 8);
  p = mbr.allocate(128, 8);
  p = mbr.allocate(128, 8);
  p = mbr.allocate(128, 8);
  mbr.deallocate(p, 1, 2);
  p = mbr.allocate(1024, 16);
  p = mbr.allocate(1024, 16);
  mbr.deallocate(p, 1, 2);
  VERIFY( r.deallocate_calls == 0 );
  mbr.release();
  VERIFY( r.deallocate_calls != 0 );
  VERIFY( r.deallocate_calls == r.allocate_calls );
  VERIFY( mbr.upstream_resource() == &r );
  VERIFY( r.number_of_active_allocations() == 0 );
}

void
test02()
{
  std::pmr::monotonic_buffer_resource mbr; // uses get_default_resource()
  auto* const upstream = mbr.upstream_resource();
  resource r;
  __gnu_test::default_resource_mgr _(&r); // calls set_default_resource(&r)
  mbr.release();
  // release() doesn't change upstream resource:
  VERIFY( mbr.upstream_resource() == upstream );
}

void
test03()
{
  resource r;
  __gnu_test::default_resource_mgr _(&r);
  std::pmr::monotonic_buffer_resource mbr(16);
  for (int i = 0; i < 100; ++i)
    (void) mbr.allocate(4, 1);
  const int allocations = r.allocate_calls;
  VERIFY( allocations != 0 );
  mbr.release();
  VERIFY( r.allocate_calls == r.deallocate_calls );
  VERIFY( r.number_of_active_allocations() == 0 );

  // next_buffer_size should have been reset to the initial value,
  // so the allocations from upstream should be the same as before.
  r.allocate_calls = 0;
  r.deallocate_calls = 0;
  for (int i = 0; i < 100; ++i)
    (void) mbr.allocate(4,1);
  VERIFY( allocations == r.allocate_calls );
}

void
test04()
{
  resource r;
  unsigned char buffer[1024];
  std::pmr::monotonic_buffer_resource mbr(buffer, sizeof(buffer), &r);
  void* p = mbr.allocate(800, 16);
  VERIFY( p >= buffer && p < (buffer + 16) );
  void* const p_in_buffer = p;
  VERIFY( r.allocate_calls == 0 );
  p = mbr.allocate(300, 1);
  VERIFY( p != buffer );
  VERIFY( p != buffer );
  VERIFY( r.allocate_calls == 1 );
  mbr.release();
  VERIFY( r.deallocate_calls == 1 );
  VERIFY( mbr.upstream_resource() == &r );
  VERIFY( r.number_of_active_allocations() == 0 );
  // initial buffer should be used again now:
  p = mbr.allocate(1000, 16);
  VERIFY( p == p_in_buffer );
  VERIFY( r.allocate_calls == 1 );
}

void
test05() // LWG 3120
{
  char buffer[100];
  {
    std::pmr::monotonic_buffer_resource mr(buffer, sizeof(buffer),
					    std::pmr::null_memory_resource());
    mr.release();
    (void) mr.allocate(60);
  }

  {
    std::pmr::monotonic_buffer_resource mr(buffer, sizeof(buffer),
					    std::pmr::null_memory_resource());
    (void) mr.allocate(60);
    mr.release();
    (void) mr.allocate(60);
  }

  {
    resource r;
    std::pmr::monotonic_buffer_resource mr(&r);
    for (int i = 0; i < 100; ++i)
    {
      (void) mr.allocate(1);
      mr.release();
    }
    VERIFY( r.number_of_active_allocations() == 0 );
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
}
