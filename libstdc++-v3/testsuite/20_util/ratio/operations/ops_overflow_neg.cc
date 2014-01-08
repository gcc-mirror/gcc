// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ratio>

void
test01()
{
  std::ratio_add<std::ratio<INTMAX_MAX, 1>, std::ratio<1>>::type r1
    __attribute__((unused));
}

void
test02()
{  
  std::ratio_multiply<std::ratio<-INTMAX_MAX, 2>, std::ratio<3, 2>>::type r1
    __attribute__((unused));
  std::ratio_multiply<std::ratio<INTMAX_MAX>, std::ratio<INTMAX_MAX>>::type r2
    __attribute__((unused));
}

// { dg-error "required from here" "" { target *-*-* } 29 }
// { dg-error "expected initializer" "" { target *-*-* } 36 }
// { dg-error "expected initializer" "" { target *-*-* } 38 }
// { dg-error "overflow in addition" "" { target *-*-* } 435 }
// { dg-error "overflow in multiplication" "" { target *-*-* } 97 }
// { dg-error "overflow in multiplication" "" { target *-*-* } 99 }
// { dg-error "overflow in multiplication" "" { target *-*-* } 101 }
// { dg-error "overflow in constant expression" "" { target *-*-* } 108 }
// { dg-prune-output "out of range" }
// { dg-prune-output "not usable in a constant expression" }
