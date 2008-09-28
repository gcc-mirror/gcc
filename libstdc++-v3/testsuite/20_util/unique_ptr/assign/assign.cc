// { dg-do compile }
// { dg-options "-std=gnu++0x" }

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

// { dg-error "used here" "" { target *-*-* } 43 }
// { dg-error "no matching" "" { target *-*-* } 49 }
// { dg-error "used here" "" { target *-*-* } 50 }
// { dg-error "candidates are" "" { target *-*-* } 215 }
// { dg-error "deleted function" "" { target *-*-* } 215 }
// { dg-error "deleted function" "" { target *-*-* } 362 }
// { dg-excess-errors "note" }
