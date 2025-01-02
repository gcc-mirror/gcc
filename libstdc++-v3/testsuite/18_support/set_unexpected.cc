// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11 -Wno-deprecated-declarations" }
// { dg-do run { target { c++11_only || c++14_only } } }

#include <exception>
#include <testsuite_hooks.h>

void unex_handler() { __builtin_abort(); }

void
test01()
{
  const std::unexpected_handler orig = std::get_unexpected();
  VERIFY( orig == std::terminate ); // GNU-specific behaviour

  std::unexpected_handler prev = std::set_unexpected(unex_handler);
  VERIFY( std::get_unexpected() == unex_handler );
  VERIFY( prev == orig );

  prev = std::set_unexpected(orig);
  VERIFY( std::get_unexpected() == orig );
  VERIFY( prev == unex_handler );
}

void
test02()
{
  // PR libstdc++/90682
  std::set_unexpected(0); // Undefined in C++98, unspecified in C++11 and C++14
  const std::unexpected_handler dfault = std::get_unexpected();
  VERIFY( dfault == std::terminate ); // GNU-specific behaviour
  const std::unexpected_handler prev = std::set_unexpected(0);
  VERIFY( prev == dfault );
}

int
main()
{
  test01();
  test02();
}
