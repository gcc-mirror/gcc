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
// { dg-require-cstdint "" }

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

  // test that it's possible to deallocate after each of the constructors
  {
    std::pmr::monotonic_buffer_resource mr(&r);
    auto p = mr.allocate(1024);
    VERIFY( p != nullptr );
    const std::uintptr_t pi = reinterpret_cast<std::uintptr_t>(p);
    mr.deallocate(p, 1024);
    VERIFY( r.deallocate_calls == 0 );
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( pi != reinterpret_cast<std::uintptr_t>(q) );
    mr.deallocate(q, 1024);
    VERIFY( r.deallocate_calls == 0 );
  }
  VERIFY( r.deallocate_calls == r.allocate_calls );
  VERIFY( r.number_of_active_allocations() == 0 );
  {
    r.deallocate_calls = r.allocate_calls = 0;
    std::pmr::monotonic_buffer_resource mr(128, &r);
    auto p = mr.allocate(64);
    VERIFY( p != nullptr );
    const std::uintptr_t pi = reinterpret_cast<std::uintptr_t>(p);
    mr.deallocate(p, 64);
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
    VERIFY( pi != reinterpret_cast<std::uintptr_t>(q) );
    mr.deallocate(q, 1024);
    VERIFY( r.deallocate_calls == 0 );
  }
  VERIFY( r.number_of_active_allocations() == 0 );
  {
    r.deallocate_calls = r.allocate_calls = 0;
    unsigned char buf[64];
    std::pmr::monotonic_buffer_resource mr((void*)buf, sizeof(buf), &r);
    auto p = mr.allocate(64);
    VERIFY( p != nullptr );
    const std::uintptr_t pi = reinterpret_cast<std::uintptr_t>(p);
    mr.deallocate(p, 64);
    auto q = mr.allocate(1024);
    VERIFY( q != nullptr );
    VERIFY( p != q );
    VERIFY( pi != reinterpret_cast<std::uintptr_t>(q) );
    mr.deallocate(q, 1024);
    VERIFY( r.deallocate_calls == 0 );
  }
  VERIFY( r.deallocate_calls == r.allocate_calls );
  VERIFY( r.number_of_active_allocations() == 0 );
}

int
main()
{
  test01();
}
