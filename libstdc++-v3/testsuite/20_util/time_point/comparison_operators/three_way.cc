// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2020 Free Software Foundation, Inc.
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

#include <chrono>
#include <testsuite_hooks.h>

// C++20 27.6.6 Comparisons [time.point.comparisons]

void
test01()
{
  using namespace std::chrono;

  auto ns = system_clock::now();
  auto s = time_point_cast<seconds>(ns + seconds(2));

  VERIFY( s != ns );
  VERIFY( std::is_lt(ns <=> s) );
}

int main()
{
  test01();
}
