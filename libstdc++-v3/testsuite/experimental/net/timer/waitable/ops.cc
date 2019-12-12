// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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
using std::error_code;

void
test01()
{
  bool test __attribute__((unused)) = false;

  io_context ctx;
  error_code ec;
  bool complete = false;

  auto then = system_timer::clock_type::now() + system_timer::duration(100);

  system_timer timer(ctx, then);
  VERIFY( timer.cancel_one() == 0 );
  VERIFY( timer.cancel() == 0 );

  timer.async_wait([&](error_code e) { ec = e; complete = true; });
  VERIFY( timer.cancel_one() == 1 );
  VERIFY( !complete );

  VERIFY( timer.cancel_one() == 0 );
  VERIFY( timer.cancel() == 0 );

  VERIFY( ctx.run() == 1 );
  VERIFY( ctx.stopped() );
  VERIFY( complete );
  VERIFY( ec == std::errc::operation_canceled );
}

void
test02()
{
  bool test __attribute__((unused)) = false;

  io_context ctx;
  error_code ec1, ec2;

  const auto now = system_timer::clock_type::now();
  const auto t1 = now + std::chrono::seconds(100);
  const auto t2 = t1 + std::chrono::seconds(100);

  system_timer timer(ctx, t1);
  VERIFY( timer.expiry() == t1 );

  VERIFY( timer.expires_at(t2) == 0 );
  VERIFY( timer.expiry() == t2 );

  timer.async_wait([&ec1](error_code e) { ec1 = e; });
  timer.async_wait([&ec2](error_code e) { ec2 = e; });
  auto n = timer.expires_at(t1);
  VERIFY( n == 2 );
  VERIFY( timer.expiry() == t1 );

  VERIFY( ctx.run_one() == 1 );
  VERIFY( ! ctx.stopped() );
  VERIFY( ctx.run_one() == 1 );
  VERIFY( ctx.stopped() );
  VERIFY( ec1 == std::errc::operation_canceled );
  VERIFY( ec2 == std::errc::operation_canceled );

  VERIFY( timer.expires_after(std::chrono::seconds(50)) == 0 );
  VERIFY( timer.expiry() < t1 );

  ec1.clear();
  ec2.clear();
  ctx.restart();
  timer.async_wait([&ec1](error_code e) { ec1 = e; });
  timer.async_wait([&ec2](error_code e) { ec2 = e; });
  VERIFY( timer.expires_after(std::chrono::seconds(10)) == 2 );
  VERIFY( timer.expiry() < t1 );
  ctx.run();
  VERIFY( ec1 == std::errc::operation_canceled );
  VERIFY( ec2 == std::errc::operation_canceled );
}

int
main()
{
  test01();
  test02();
}
