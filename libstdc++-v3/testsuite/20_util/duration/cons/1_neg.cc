// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// Copyright (C) 2008 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

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

// { dg-error "instantiated from here" "" { target *-*-* } 30 }
// { dg-error "instantiated from here" "" { target *-*-* } 39 }
// { dg-error "not exactly representable" "" { target *-*-* } 218 }
// { dg-error "integral duration with floating point" "" { target *-*-* } 208 }
// { dg-excess-errors "In instantiation of" }
