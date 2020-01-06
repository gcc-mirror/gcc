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

namespace net = std::experimental::net;
using net::ip::address_v4;

void
test01()
{
  bool test __attribute__((unused)) = false;

  auto a0 = make_address_v4( address_v4::bytes_type{} );
  VERIFY( a0.to_uint() == 0 );
  VERIFY( a0.to_bytes() == address_v4::bytes_type{} );

  address_v4::bytes_type b1{ 1, 2, 3, 4 };
  auto a1 = make_address_v4( b1 );
  VERIFY( a1.to_uint() == ntohl((1 << 24) | (2 << 16) | (3 << 8) | 4) );
  VERIFY( a1.to_bytes() == b1 );
}

void
test02()
{
  bool test __attribute__((unused)) = false;

  auto a0 = net::ip::make_address_v4(0u);
  VERIFY( a0.to_uint() == 0 );
  VERIFY( a0.to_bytes() == address_v4::bytes_type{} );

  address_v4::uint_type u1 = ntohl((5 << 24) | (6 << 16) | (7 << 8) | 8);
  auto a1 = net::ip::make_address_v4( u1 );
  VERIFY( a1.to_uint() == u1 );
  VERIFY( a1.to_bytes() == address_v4::bytes_type( 5, 6, 7, 8 ) );
}

void
test03()
{
  bool test __attribute__((unused)) = false;

  auto a1 = net::ip::make_address_v4("127.0.0.1");
  VERIFY( a1.is_loopback() );
  auto a2 = net::ip::make_address_v4(std::string{"127.0.0.2"});
  VERIFY( a2.is_loopback() );
  auto a3 = net::ip::make_address_v4(std::experimental::string_view{"127.0.0.3"});
  VERIFY( a3.is_loopback() );

  std::error_code ec;
  auto a4 = net::ip::make_address_v4("127...1", ec);
  VERIFY( ec == std::errc::invalid_argument );

  net::ip::make_address_v4("127.0.0.1", ec);
  VERIFY( !ec );

  a4 = net::ip::make_address_v4(std::string{"256.0.0.1"}, ec);
  VERIFY( ec == std::errc::invalid_argument );

  net::ip::make_address_v4(std::string{"127.0.0.1"}, ec);
  VERIFY( !ec );

  a4 = net::ip::make_address_v4(std::experimental::string_view{""}, ec);
  VERIFY( ec == std::errc::invalid_argument );
}

int
main()
{
  test01();
  test02();
  test03();
}
