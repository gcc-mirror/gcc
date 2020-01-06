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
// { dg-add-options net_ts }

#include <experimental/internet>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = false;

  using resolver = std::experimental::net::ip::resolver_base;

  resolver::flags f = resolver::passive;

  VERIFY( (f & resolver::numeric_host) == 0);
  f &= resolver::numeric_host;
  VERIFY( f == 0 );

  VERIFY( (f | resolver::numeric_host) == resolver::numeric_host);
  f |= resolver::numeric_host;
  VERIFY( f == resolver::numeric_host );

  VERIFY( (f ^ resolver::numeric_host) == 0 );
  f ^= resolver::numeric_host;
  VERIFY( f == 0 );

  f = ~resolver::numeric_host;
  VERIFY( (f & resolver::numeric_host) == 0);
  VERIFY( (f | resolver::numeric_host) == ~resolver::flags{} );

  (void) resolver::passive;
  (void) resolver::canonical_name;
  (void) resolver::numeric_host;
#ifdef AI_NUMERICSERV
  (void) resolver::numeric_service;
#endif
  (void) resolver::v4_mapped;
  (void) resolver::all_matching;
  (void) resolver::address_configured;
}

int
main()
{
  test01();
}
