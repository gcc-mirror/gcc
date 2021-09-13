// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

  io_context ctx1, ctx2;

  system_timer timer1(ctx1);
  VERIFY( timer1.get_executor() == ctx1.get_executor() );
  VERIFY( timer1.expiry() == system_timer::time_point() );

  system_timer timer2(ctx2);
  VERIFY( timer2.get_executor() == ctx2.get_executor() );
  VERIFY( timer2.get_executor() != timer1.get_executor() );
  VERIFY( timer2.expiry() == system_timer::time_point() );

  system_timer timer3(std::move(timer1));
  VERIFY( timer3.get_executor() == ctx1.get_executor() );
  VERIFY( timer3.expiry() == system_timer::time_point() );
  VERIFY( timer1.expiry() == system_timer::time_point() );

  system_timer timer4(std::move(timer2));
  VERIFY( timer4.get_executor() == ctx2.get_executor() );
  VERIFY( timer4.expiry() == system_timer::time_point() );
  VERIFY( timer2.expiry() == system_timer::time_point() );
}

void
test02()
{
  bool test __attribute__((unused)) = false;

  io_context ctx1, ctx2;
  auto t1 = system_timer::clock_type::now();
  auto t2 = t1 + system_timer::duration(10);

  system_timer timer1(ctx1, t1);
  VERIFY( timer1.get_executor() == ctx1.get_executor() );
  VERIFY( timer1.expiry() == t1 );

  system_timer timer2(ctx2, t2);
  VERIFY( timer2.get_executor() == ctx2.get_executor() );
  VERIFY( timer2.get_executor() != timer1.get_executor() );
  VERIFY( timer2.expiry() == t2 );

  system_timer timer3(std::move(timer1));
  VERIFY( timer3.get_executor() == ctx1.get_executor() );
  VERIFY( timer3.expiry() == t1 );
  VERIFY( timer1.expiry() == system_timer::time_point() );

  system_timer timer4(std::move(timer2));
  VERIFY( timer4.get_executor() == ctx2.get_executor() );
  VERIFY( timer4.expiry() == t2 );
  VERIFY( timer2.expiry() == system_timer::time_point() );
}

void
test03()
{
  bool test __attribute__((unused)) = false;

  io_context ctx1, ctx2;
  auto now = system_timer::clock_type::now();
  auto d1 = system_timer::duration(10);
  auto d2 = system_timer::duration(100);

  system_timer timer1(ctx1, d1);
  VERIFY( timer1.get_executor() == ctx1.get_executor() );
  VERIFY( timer1.expiry() > now );

  system_timer timer2(ctx2, d2);
  VERIFY( timer2.get_executor() == ctx2.get_executor() );
  VERIFY( timer2.get_executor() != timer1.get_executor() );
  VERIFY( timer2.expiry() > now );
  VERIFY( timer2.expiry() != timer1.expiry() );

  system_timer timer3(std::move(timer1));
  VERIFY( timer3.get_executor() == ctx1.get_executor() );
  VERIFY( timer3.expiry() > now );
  VERIFY( timer1.expiry() == system_timer::time_point() );

  system_timer timer4(std::move(timer2));
  VERIFY( timer4.get_executor() == ctx2.get_executor() );
  VERIFY( timer4.expiry() > now );
  VERIFY( timer2.expiry() == system_timer::time_point() );
}

int
main()
{
  test01();
  test02();
  test03();
}
