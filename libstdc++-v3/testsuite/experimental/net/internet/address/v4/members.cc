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

using std::experimental::net::ip::address_v4;

void
test01()
{
  bool test __attribute__((unused)) = false;

  address_v4 a;
  VERIFY( a.is_unspecified() );

  a = address_v4::any();
  VERIFY( a.is_unspecified() );

  a = address_v4::loopback();
  VERIFY( !a.is_unspecified() );

  a = address_v4::broadcast();
  VERIFY( !a.is_unspecified() );
}

void
test02()
{
  bool test __attribute__((unused)) = false;

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
}

void
test03()
{
  bool test __attribute__((unused)) = false;

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
}

void
test04()
{
  bool test __attribute__((unused)) = false;

  VERIFY( address_v4::any().to_string() == "0.0.0.0" );
  VERIFY( address_v4::loopback().to_string() == "127.0.0.1" );
  VERIFY( address_v4::broadcast().to_string() == "255.255.255.255" );
}

void
test05()
{
  bool test __attribute__((unused)) = false;

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
