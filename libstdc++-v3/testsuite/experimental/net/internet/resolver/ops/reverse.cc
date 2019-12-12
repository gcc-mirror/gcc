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
// { dg-add-options net_ts }

#include <experimental/internet>
#include <testsuite_hooks.h>

using namespace std::experimental::net;

void
test01()
{
  bool test __attribute__((unused)) = false;

  std::error_code ec;
  io_context ctx;
  ip::tcp::resolver resolv(ctx);
  ip::tcp::endpoint home{ip::address_v4::loopback(), 80};
  auto addrs = resolv.resolve(home, ec);
  VERIFY( !ec );
  VERIFY( addrs.size() == 1 );
  VERIFY( addrs.begin() != addrs.end() );
  VERIFY( ! addrs.empty() );

  auto addrs2 = resolv.resolve(home);
  VERIFY( addrs == addrs2 );
}

int
main()
{
  test01();
}
