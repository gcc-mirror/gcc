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
  auto addrs = resolv.resolve("localhost", "http", ec);
  VERIFY( !ec );
  VERIFY( addrs.size() > 0 );
  VERIFY( addrs.begin() != addrs.end() );
  VERIFY( ! addrs.empty() );

  auto addrs2 = resolv.resolve("localhost", "http");
  VERIFY( addrs == addrs2 );
}

void
test02()
{
  bool test __attribute__((unused)) = false;

  std::error_code ec;
  io_context ctx;
  ip::tcp::resolver resolv(ctx);
  auto flags = ip::resolver_base::numeric_host;
#ifdef AI_NUMERICSERV
  flags |= ip::tcp::resolver::numeric_service;
#endif
  auto addrs = resolv.resolve("127.0.0.1", "42", flags, ec);
  VERIFY( !ec );
  VERIFY( addrs.size() > 0 );
  VERIFY( addrs.begin() != addrs.end() );

  auto addrs2 = resolv.resolve("127.0.0.1", "42", flags);
  VERIFY( addrs == addrs2 );

  addrs = resolv.resolve("localhost", "42", flags, ec);
  VERIFY( ec );
  VERIFY( addrs.empty() );
  addrs = resolv.resolve("127.0.0.1", "nameserver", flags, ec);
  VERIFY( ec );
  VERIFY( addrs.empty() );

#if __cpp_exceptions
  bool caught = false;
  try {
    resolv.resolve("localhost", "http", flags);
  } catch (const std::system_error& e) {
    caught = true;
    VERIFY( e.code() == ec );
  }
  VERIFY( caught );
#endif
}

void
test03()
{
  bool test __attribute__((unused)) = false;

  std::error_code ec;
  io_context ctx;
  ip::tcp::resolver resolv(ctx);
  auto addrs = resolv.resolve("test.invalid", "http", ec);
  VERIFY( ec );
  VERIFY( addrs.size() == 0 );
  VERIFY( addrs.begin() == addrs.end() );
  VERIFY( addrs.empty() );
#if __cpp_exceptions
  bool caught = false;
  try {
    resolv.resolve("test.invalid", "http");
  } catch (const std::system_error& e) {
    caught = true;
    VERIFY( e.code() == ec );
  }
  VERIFY( caught );
#endif
}

int
main()
{
  test01();
  test02();
  test03();
}
