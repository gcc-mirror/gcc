// Copyright (C) 2021-2023 Free Software Foundation, Inc.
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
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <sstream>
#include <testsuite_hooks.h>

using std::experimental::net::ip::address_v6;

constexpr bool
test01()
{
  address_v6 a;
  VERIFY( a.is_unspecified() );
  VERIFY( !a.is_loopback() );
  VERIFY( !a.is_multicast() );
  VERIFY( !a.is_link_local() );
  VERIFY( !a.is_site_local() );

  a = address_v6::any();
  VERIFY( a.is_unspecified() );
  VERIFY( !a.is_loopback() );
  VERIFY( !a.is_multicast() );
  VERIFY( !a.is_link_local() );
  VERIFY( !a.is_site_local() );

  a = address_v6::loopback();
  VERIFY( !a.is_unspecified() );
  VERIFY( a.is_loopback() );
  VERIFY( !a.is_multicast() );
  VERIFY( !a.is_link_local() );
  VERIFY( !a.is_site_local() );

  a = address_v6{address_v6::loopback().to_bytes(), 1};
  VERIFY( !a.is_unspecified() );
  VERIFY( !a.is_loopback() );
  VERIFY( !a.is_multicast() );
  VERIFY( !a.is_link_local() );
  VERIFY( !a.is_site_local() );

  return true;
}

static_assert(test01(), "");

constexpr bool
test02()
{
  auto a = address_v6{address_v6::bytes_type{0xFF}};
  VERIFY( a.is_multicast() );

  a = address_v6{address_v6::bytes_type{0xFF, 0x01}};
  VERIFY( a.is_multicast() );

  a = address_v6{address_v6::bytes_type{0xFF, 0x00, 0x01}};
  VERIFY( a.is_multicast() );

  a = address_v6{address_v6::bytes_type{0xFE, 0x80}};
  VERIFY( !a.is_multicast() );

  a = address_v6{address_v6::bytes_type{0xFE, 0xC0}};
  VERIFY( !a.is_multicast() );

  return true;
}

static_assert(test02(), "");

void
test03()
{
  // Assume these addresses use the preferred forms:
  VERIFY( address_v6::any().to_string() == "::" );
  VERIFY( address_v6::loopback().to_string() == "::1" );

  // Choose values with no leading zeros, so output is portable:
  address_v6 a{address_v6::bytes_type{21,22,23,24,25,26,27,28,29}, 42};
  const std::string s = a.to_string();
  if (s.find("::") != s.npos)
    VERIFY( s == "1516:1718:191a:1b1c:1d00::%42" );
  else
  {
    // Contiguous zeros were not shortened to "::"
    VERIFY( s.substr(0, 25) == "1516:1718:191a:1b1c:1d00:" );
    VERIFY( s.substr(s.size() - 3) == "%42" );
  }
}

void
test04()
{
  address_v6 a{address_v6::bytes_type{1,2,3,4,5,6,7,8,9}, 42};
  std::ostringstream ss;
  ss << address_v6::any() << ' ' << address_v6::loopback() << ' ' << a;
  VERIFY( ss.str() == (":: ::1 " + a.to_string()) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
