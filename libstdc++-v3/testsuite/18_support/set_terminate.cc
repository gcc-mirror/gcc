// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }
// { dg-do run { target c++11 } }

#include <exception>
#include <testsuite_hooks.h>

void term_handler() { __builtin_abort(); }

void
test01()
{
  const std::terminate_handler orig = std::get_terminate();
  VERIFY( orig != 0 ); // GNU-specific behaviour

  std::terminate_handler prev = std::set_terminate(term_handler);
  VERIFY( std::get_terminate() == term_handler );
  VERIFY( prev == orig );

  prev = std::set_terminate(orig);
  VERIFY( std::get_terminate() == orig );
  VERIFY( prev == term_handler );
}

void
test02()
{
  // PR libstdc++/90682
  std::set_terminate(0); // Undefined in C++98, unspecified in C++11 and later
  const std::terminate_handler dfault = std::get_terminate();
  VERIFY( dfault != 0 ); // GNU-specific behaviour
  const std::terminate_handler prev = std::set_terminate(0);
  VERIFY( prev == dfault );
}

int
main()
{
  test01();
  test02();
}
