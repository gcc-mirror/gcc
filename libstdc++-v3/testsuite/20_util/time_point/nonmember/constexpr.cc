// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

int main()
{
  using namespace std::chrono;
  
  typedef time_point<system_clock> time_type;

  constexpr time_type t1(seconds(1));
  constexpr time_type t2(seconds(30));
  constexpr time_type t3(seconds(60));
  
  constexpr duration<int> d0(12);
  constexpr duration<int> d1(3);

  constexpr auto r1 __attribute__((unused)) = t1 + d0;
  constexpr auto r2 __attribute__((unused)) = d1 + t2;

  constexpr auto r3 __attribute__((unused)) = t1 - d0;
  constexpr auto r4 __attribute__((unused)) = t2 - t3;

  return 0;
}
