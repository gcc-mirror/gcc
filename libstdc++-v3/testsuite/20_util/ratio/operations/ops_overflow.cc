// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

#include <ratio>

void
test01()
{
  std::ratio_add<std::ratio<INTMAX_MAX, 1>, std::ratio<1>>::type r1;
}

void
test02()
{  
  std::ratio_multiply<std::ratio<-INTMAX_MAX, 2>, std::ratio<3, 2>>::type r1;
  std::ratio_multiply<std::ratio<INTMAX_MAX>, std::ratio<INTMAX_MAX>>::type r2;
}

// { dg-error "instantiated from here" "" { target *-*-* } 30 }
// { dg-error "instantiated from here" "" { target *-*-* } 36 }
// { dg-error "instantiated from here" "" { target *-*-* } 37 }
// { dg-error "overflow in addition" "" { target *-*-* } 127 }
// { dg-error "overflow in multiplication" "" { target *-*-* } 95 }
// { dg-error "overflow in multiplication" "" { target *-*-* } 97 }
// { dg-error "overflow in multiplication" "" { target *-*-* } 99 }
// { dg-excess-errors "In instantiation of" }
// { dg-excess-errors "out of range" }
