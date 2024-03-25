// { dg-do run { target c++11 } }

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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


// NOTE: This makes use of the fact that we know how moveable
// is implemented on tuple.  If the implementation changed
// this test may begin to fail.

#include <tuple>
#include <utility>
#include <testsuite_hooks.h>

int main()
{
  std::tuple<int, double> a(1, 2.0), b;
  b = std::move(a);
  VERIFY( std::get<0>(b) == 1 && std::get<1>(b) == 2.0 );
  VERIFY( std::get<0>(a) == 1 && std::get<1>(a) == 2.0 );

  std::tuple<int, double> c(std::move(b));
  VERIFY( std::get<0>(c) == 1 && std::get<1>(c) == 2.0 );
  VERIFY( std::get<0>(b) == 1 && std::get<1>(b) == 2.0 );
  return 0;
}
