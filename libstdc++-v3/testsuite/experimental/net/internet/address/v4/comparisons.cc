// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

using std::experimental::net::ip::address_v4;

void
test01()
{
  address_v4 addrs[] = {
    address_v4::any(), address_v4::loopback(), address_v4::broadcast(),
    address_v4{0x11001100}, address_v4{0xEFEFEFEF}
  };

  auto begin = std::begin(addrs);
  auto end = std::end(addrs);
  for (auto it = begin; it != end; ++it)
  {
    auto& a = *it;
    VERIFY( a == a );
    VERIFY( a <= a );
    VERIFY( a >= a );
    VERIFY( ! (a != a) );
    VERIFY( ! (a < a) );
    VERIFY( ! (a > a) );
  }

  std::sort(begin, end);

  for (auto it = begin + 1; it != end; ++it)
  {
    auto& a = *it;
    auto& b = *begin;
    VERIFY( ! (a == b) );
    VERIFY( a != b );
    VERIFY( b < a );
    VERIFY( b <= a );
    VERIFY( a > b );
    VERIFY( a >= b );
  }
}

int
main()
{
  test01();
}
