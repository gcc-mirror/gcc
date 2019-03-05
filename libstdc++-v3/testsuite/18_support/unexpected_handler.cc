// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// D.11 Violating exception-specifications

#include <exception>
#include <cstdlib>
#include <testsuite_hooks.h>

void handler() { std::abort(); }

void test01()
{
  auto prev = std::set_unexpected(handler);
  VERIFY( prev != handler );
  auto curr = std::get_unexpected();
  VERIFY( curr == handler );
}

int main()
{
  test01();
  return 0;
}
