// { dg-do compile { target c++11 } }

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// 20.8.3.1 duration constructors [time.duration.cons]

#include <chrono>

void
test01()
{
  std::chrono::duration<int> d1(1.0); // { dg-error "no matching|no type" }
}

void
test02()
{
  using namespace std::chrono;
  
  duration<int, std::micro> d2(8);
  duration<int, std::milli> d2_copy(d2); // { dg-error "no matching|no type" }
}

// { dg-prune-output "include" }

// Discard a bogus warning showing up with -Wall.
// { dg-prune-output "suggest parentheses around" }
