// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/socket>
#include <testsuite_common_types.h>

using S = std::experimental::net::socket_base;
using namespace std;

void test_constants()
{
  static_assert( is_enum<S::shutdown_type>::value, "" );
  static_assert( S::shutdown_receive != S::shutdown_send, "" );
  static_assert( S::shutdown_receive != S::shutdown_both, "" );
  static_assert( S::shutdown_send != S::shutdown_both, "" );

  static_assert( is_enum<S::wait_type>::value, "" );
  static_assert( S::wait_read != S::wait_write, "");
  static_assert( S::wait_read != S::wait_error, "");
  static_assert( S::wait_write != S::wait_error, "");

  static_assert( __gnu_test::test_bitmask_values(
	{S::message_peek, S::message_out_of_band, S::message_do_not_route}
	), "each bitmask element is distinct" );

  auto m = &S::max_listen_connections;
  static_assert( is_same<decltype(m), const int*>::value, "" );
}

