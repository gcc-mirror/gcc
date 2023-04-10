// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
#include <testsuite_allocator.h>

using std::experimental::net::ip::address_v4;

static_assert(std::is_standard_layout<address_v4::bytes_type>::value,
    "net::ip::address_v4::bytes_type is a standard layout type");

constexpr bool
test01()
{
  address_v4 a;
  VERIFY( a.is_unspecified() );

  a = address_v4::any();
  VERIFY( a.is_unspecified() );

  a = address_v4::loopback();
  VERIFY( !a.is_unspecified() );

  a = address_v4::broadcast();
  VERIFY( !a.is_unspecified() );

  return true;
}

static_assert(test01(), "");

constexpr bool
test02()
{
  auto a = address_v4::loopback();
  VERIFY( a.is_loopback() );

  a = address_v4{0x7F000001};
  VERIFY( a.is_loopback() );

  a = address_v4{0x7F010203};
  VERIFY( a.is_loopback() );

  a = address_v4{0x7FFFFFFF};
  VERIFY( a.is_loopback() );

  a = address_v4::any();
  VERIFY( !a.is_loopback() );

  a = address_v4::broadcast();
  VERIFY( !a.is_loopback() );

  return true;
}

static_assert(test02(), "");

constexpr bool
test03()
{
  auto a = address_v4{0xE0000001};
  VERIFY( a.is_multicast() );

  a = address_v4{0xE0010203};
  VERIFY( a.is_multicast() );

  a = address_v4{0xE0FFFFFF};
  VERIFY( a.is_multicast() );

  a = address_v4{0xF0000000};
  VERIFY( !a.is_multicast() );

  a = address_v4{0xDFFFFFFF};
  VERIFY( !a.is_multicast() );

  return true;
}

static_assert(test03(), "");

void
test04()
{
  VERIFY( address_v4::any().to_string() == "0.0.0.0" );
  VERIFY( address_v4::loopback().to_string() == "127.0.0.1" );
  VERIFY( address_v4::broadcast().to_string() == "255.255.255.255" );
  using b = address_v4::bytes_type;
  VERIFY( address_v4(b(1, 23, 45, 67)).to_string() == "1.23.45.67" );
  VERIFY( address_v4(b(12, 34, 56, 78)).to_string() == "12.34.56.78" );
  VERIFY( address_v4(b(123, 4, 5, 6)).to_string() == "123.4.5.6" );
  VERIFY( address_v4(b(123, 234, 124, 235)).to_string() == "123.234.124.235" );

  __gnu_test::uneq_allocator<char> alloc(123);
  auto str = address_v4(b(12, 34, 56, 78)).to_string(alloc);
  VERIFY(str.get_allocator().get_personality() == alloc.get_personality());
  VERIFY( str == "12.34.56.78" );
}

void
test05()
{
  std::ostringstream ss;
  ss << address_v4::any() << ' ' << address_v4::loopback() << ' '
    << address_v4::broadcast();
  VERIFY( ss.str() == "0.0.0.0 127.0.0.1 255.255.255.255" );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
