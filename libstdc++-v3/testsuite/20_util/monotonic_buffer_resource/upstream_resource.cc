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

void
test01()
{
  __gnu_test::memory_resource r;
  const auto null = std::pmr::null_memory_resource();
  const auto newdel = std::pmr::new_delete_resource();
  std::pmr::set_default_resource(null);

  {
    std::pmr::monotonic_buffer_resource mr(&r);
    VERIFY( mr.upstream_resource() == &r );
    __gnu_test::default_resource_mgr _(newdel);
    VERIFY( mr.upstream_resource() == &r );
  }
  {
    std::pmr::monotonic_buffer_resource mr(128, &r);
    VERIFY( mr.upstream_resource() == &r );
    __gnu_test::default_resource_mgr _(newdel);
    VERIFY( mr.upstream_resource() == &r );
  }
  {
    unsigned char buf[64];
    std::pmr::monotonic_buffer_resource mr((void*)buf, sizeof(buf), &r);
    VERIFY( mr.upstream_resource() == &r );
    __gnu_test::default_resource_mgr _(newdel);
    VERIFY( mr.upstream_resource() == &r );
  }
  {
    std::pmr::monotonic_buffer_resource mr;
    VERIFY( mr.upstream_resource() == null );
    __gnu_test::default_resource_mgr _(newdel);
    VERIFY( mr.upstream_resource() == null );
  }
  {
    std::pmr::monotonic_buffer_resource mr(64);
    VERIFY( mr.upstream_resource() == null );
    __gnu_test::default_resource_mgr _(newdel);
    VERIFY( mr.upstream_resource() == null );
  }
  {
    unsigned char buf[64];
    std::pmr::monotonic_buffer_resource mr((void*)buf, sizeof(buf));
    VERIFY( mr.upstream_resource() == null );
    __gnu_test::default_resource_mgr _(newdel);
    VERIFY( mr.upstream_resource() == null );
  }
}

int
main()
{
  test01();
}
