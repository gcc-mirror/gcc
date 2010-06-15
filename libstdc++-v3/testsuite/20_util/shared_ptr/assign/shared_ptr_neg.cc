// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation
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

// 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

struct A { };
struct B { };

// 20.6.6.2.3 shared_ptr assignment [util.smartptr.shared.assign]

// Assignment from incompatible shared_ptr<Y>
int
test01()
{
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> a;
  std::shared_ptr<B> b;
  a = b;                      // { dg-error "here" }

  return 0;
}

int 
main()
{
  test01();
  return 0;
}
// { dg-error "In member function" "" { target *-*-* } 0 }
// { dg-error "cannot convert" "" { target *-*-* } 0 }
// { dg-error "instantiated from" "" { target *-*-* } 0 }
