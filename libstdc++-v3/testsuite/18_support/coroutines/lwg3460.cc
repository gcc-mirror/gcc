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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <coroutine>

void
test01()
{
  // LWG 3460. Unimplementable noop_coroutine_handle guarantees

  static_assert( std::is_convertible_v<std::noop_coroutine_handle&,
				       std::coroutine_handle<>> );
  static_assert( ! std::is_convertible_v<std::noop_coroutine_handle&,
					 std::coroutine_handle<>&> );
  static_assert( ! std::is_assignable_v<std::noop_coroutine_handle&,
					std::coroutine_handle<>> );

  std::noop_coroutine_handle h = std::noop_coroutine();
  std::coroutine_handle<> h2 = h;
  h2();		// no-op
  h2.resume();	// no-op
  h2.destroy();	// no-op
}

void
test02()
{
  // LWG 3469. Precondition of coroutine_handle::promise may be insufficient

  struct P1 { };
  struct P2 { };

  static_assert( ! std::is_assignable_v<std::coroutine_handle<P1>&,
					std::coroutine_handle<>> );
  static_assert( ! std::is_assignable_v<std::coroutine_handle<P1>&,
					std::coroutine_handle<P2>> );
}
