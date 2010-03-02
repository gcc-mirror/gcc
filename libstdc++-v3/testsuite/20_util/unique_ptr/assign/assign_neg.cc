// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009, 2010 Free Software Foundation
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

#include <memory>

struct base { virtual ~base() {} };
struct derived : base {};

void
test01()
{
  std::unique_ptr<derived> p1(new derived);
  std::unique_ptr<derived> p2(new derived);
//  p2 = p1;  // should not compile
  p2 = std::move(p1);
  std::unique_ptr<base> p3(new base);
//  p3 = p2;  // should not compile
  p3 = std::move(p2);
}

void
test02()
{
  std::unique_ptr<int[]> p1(new int(420));
  std::unique_ptr<int[]> p2 = p1;
}

void
test03()
{
  std::unique_ptr<int[2]> p1(new int[3]);
  std::unique_ptr<int[2]> p2 = p1;
}

// { dg-error "deleted function" "" { target *-*-* } 346 }
// { dg-error "used here" "" { target *-*-* } 42 }
// { dg-error "no matching" "" { target *-*-* } 48 }
// { dg-warning "candidates are" "" { target *-*-* } 115 }
// { dg-warning "note" "" { target *-*-* } 108 }
// { dg-warning "note" "" { target *-*-* } 103 }
// { dg-warning "note" "" { target *-*-* } 98 }
// { dg-warning "note" "" { target *-*-* } 92 }
// { dg-error "deleted function" "" { target *-*-* } 209 }
// { dg-error "used here" "" { target *-*-* } 49 }
