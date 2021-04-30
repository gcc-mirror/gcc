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

// { dg-do run { target c++14 } }

#include <experimental/socket>
#include <testsuite_common_types.h>
#include <testsuite_hooks.h>

using S = std::experimental::net::socket_base;
using namespace std;

static_assert( ! is_default_constructible<S>(), "protected" );
static_assert( ! is_destructible<S>(), "protected" );
struct Sock : S { };
static_assert( is_default_constructible<Sock>(), "" );
static_assert( is_destructible<Sock>(), "" );

// Dummy protocol
struct P
{
  struct endpoint
  {
    using protocol_type = P;
    P protocol() const;
  };
};

template<typename C, typename T>
void check_gettable_sockopt()
{
  P p;
  static_assert( is_same<decltype(declval<const C&>().level(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().level(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().name(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().name(p)), "" );

  static_assert( is_same<decltype(declval<C&>().data(p)), void*>(), "" );
  static_assert( noexcept(declval<C&>().data(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().size(p)), size_t>(), "" );
  static_assert( noexcept(declval<const C&>().size(p)), "" );

  static_assert( is_same<decltype(declval<C&>().resize(p, 0)), void>(), "" );
  static_assert( ! noexcept(declval<C&>().resize(p, 0)), "" );

  C opt;
  VERIFY(opt.size(p) == sizeof(T));
}

template<typename C, typename T>
void check_settable_sockopt()
{
  P p;
  static_assert( is_same<decltype(declval<const C&>().level(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().level(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().name(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().name(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().data(p)), const void*>(), "" );
  static_assert( noexcept(declval<const C&>().data(p)), "" );

  static_assert( is_same<decltype(declval<C&>().size(p)), size_t>(), "" );
  static_assert( noexcept(declval<C&>().size(p)), "" );

  C opt;
  VERIFY(opt.size(p) == sizeof(T));
}

template<typename C, typename T = int>
void check_boolean_sockopt()
{
  check_gettable_sockopt<C, T>();
  check_settable_sockopt<C, T>();

  static_assert( is_destructible<C>(), "" );
  static_assert( is_nothrow_default_constructible<C>(), "" );
  static_assert( is_nothrow_copy_constructible<C>(), "" );
  static_assert( is_nothrow_copy_assignable<C>(), "" );

  static_assert( is_nothrow_constructible<C, bool>(), "" );
  static_assert( ! is_convertible<bool, C>(), "constructor is explicit" );
  static_assert( is_nothrow_assignable<C&, bool>(), "" );

  static_assert( is_same<decltype(declval<const C&>().value()), bool>(), "" );
  static_assert( noexcept(declval<const C&>().value()), "" );

  static_assert( is_same<decltype(static_cast<bool>(declval<const C&>())), bool>(), "" );
  static_assert( noexcept(static_cast<bool>(declval<const C&>())), "" );

  static_assert( is_same<decltype(!declval<const C&>()), bool>(), "" );
  static_assert( noexcept(!declval<const C&>()), "" );
}

template<typename C, typename T = int>
void check_integer_sockopt()
{
  check_gettable_sockopt<C, T>();
  check_settable_sockopt<C, T>();

  static_assert( is_destructible<C>(), "" );
  static_assert( is_nothrow_default_constructible<C>(), "" );
  static_assert( is_nothrow_copy_constructible<C>(), "" );
  static_assert( is_nothrow_copy_assignable<C>(), "" );

  static_assert( is_nothrow_constructible<C, int>(), "" );
  static_assert( ! is_convertible<int, C>(), "constructor is explicit" );
  static_assert( is_nothrow_assignable<C&, int>(), "" );

  static_assert( is_same<decltype(declval<const C&>().value()), int>(), "" );
  static_assert( noexcept(declval<const C&>().value()), "" );
}

void test_option_types()
{
#if __has_include(<sys/socket.h>)
  check_boolean_sockopt<S::broadcast>();

  check_boolean_sockopt<S::debug>();

  check_boolean_sockopt<S::do_not_route>();

  check_boolean_sockopt<S::keep_alive>();

  check_gettable_sockopt<S::linger, ::linger>();
  check_settable_sockopt<S::linger, ::linger>();
  static_assert( is_destructible<S::linger>(), "" );
  static_assert( is_nothrow_default_constructible<S::linger>(), "" );
  static_assert( is_nothrow_copy_constructible<S::linger>(), "" );
  static_assert( is_nothrow_copy_assignable<S::linger>(), "" );
  static_assert( is_nothrow_constructible<S::linger, bool, chrono::seconds>(), "" );

  static_assert( is_same<decltype(declval<const S::linger&>().enabled()), bool>(), "" );
  static_assert( noexcept(declval<const S::linger&>().enabled()), "" );

  static_assert( is_void<decltype(declval<S::linger&>().enabled(true))>(), "" );
  static_assert( noexcept(declval<S::linger&>().enabled(true)), "" );

  static_assert( is_same<decltype(declval<const S::linger&>().timeout()), chrono::seconds>(), "" );
  static_assert( noexcept(declval<const S::linger&>().timeout()), "" );

  static_assert( is_void<decltype(declval<S::linger&>().timeout(chrono::seconds()))>(), "" );
  static_assert( noexcept(declval<S::linger&>().timeout(chrono::seconds())), "" );

  check_boolean_sockopt<S::out_of_band_inline>();

  check_integer_sockopt<S::receive_buffer_size>();

  check_integer_sockopt<S::receive_low_watermark>();

  check_boolean_sockopt<S::reuse_address>();

  check_integer_sockopt<S::send_buffer_size>();

  check_integer_sockopt<S::send_low_watermark>();
#endif
}

void test_constants()
{
#if __has_include(<sys/socket.h>)
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
#endif
}

int main()
{
  test_option_types();
}
