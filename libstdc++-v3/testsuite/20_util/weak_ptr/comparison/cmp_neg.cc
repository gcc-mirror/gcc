// { dg-options "-std=gnu++0x " }
// { dg-do compile }

// Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation
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

// 20.6.6.3 Template class weak_ptr [util.smartptr.weak]

#include <memory>

struct A { };

// 20.8.13.3.6 weak_ptr comparison [util.smartptr.weak.cmp] (removed)

int
test01()
{
  std::weak_ptr<A> p1;
  p1 < p1;  // { dg-error "no match" }
  // { dg-error "candidate" "candidate note" { target *-*-* } 33 }
  return 0;
}

int 
main()
{
  test01();
  return 0;
}

// { dg-warning "note" "" { target *-*-* } 354 }
// { dg-warning "note" "" { target *-*-* } 1086 }
// { dg-warning "note" "" { target *-*-* } 469 }
// { dg-warning "note" "" { target *-*-* } 587 }
// { dg-warning "note" "" { target *-*-* } 1050 }
// { dg-warning "note" "" { target *-*-* } 1056 }
// { dg-warning "note" "" { target *-*-* } 342 }
// { dg-warning "note" "" { target *-*-* } 292 }
// { dg-warning "note" "" { target *-*-* } 207 }
