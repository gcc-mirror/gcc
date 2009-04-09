// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// Copyright (C) 2008, 2009 Free Software Foundation
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
  std::chrono::duration<int> d1(1.0);
}

void
test02()
{
  using namespace std::chrono;
  
  duration<int, std::micro> d2(8);
  duration<int, std::milli> d2_copy(d2);
}

// { dg-error "instantiated from here" "" { target *-*-* } 29 }
// { dg-error "instantiated from here" "" { target *-*-* } 38 }
// { dg-error "not exactly representable" "" { target *-*-* } 227 }
// { dg-error "integral duration with floating point" "" { target *-*-* } 217 }
// { dg-excess-errors "In instantiation of" }
