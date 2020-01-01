// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }
// { dg-add-options libatomic }

#include <experimental/timer>
#include <testsuite_hooks.h>

using std::experimental::net::system_timer;
using std::experimental::net::io_context;

void
test01()
{
  bool test __attribute__((unused)) = false;

  std::error_code ec;

  io_context ctx;
  {
    system_timer timer(ctx, system_timer::duration(3600));
    timer.async_wait([&ec](std::error_code e) { ec = e; });
  }
  auto n = ctx.run();
  __builtin_printf("ran %lu\n", n);
  VERIFY( n == 1 );
  VERIFY( ec == std::errc::operation_canceled );
}

int
main()
{
  test01();
}
