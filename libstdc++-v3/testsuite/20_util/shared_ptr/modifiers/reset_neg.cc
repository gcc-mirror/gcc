// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2006, 2007, 2009 Free Software Foundation
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

// 20.6.6.2.4 shared_ptr modifiers [util.smartptr.shared.mod]

// reset
int
test01()
{
  bool test __attribute__((unused)) = true;

  const std::shared_ptr<A> p1(new A);
  p1.reset();     // { dg-error "discards qualifiers" }

  return 0;
}

int
main()
{
  test01();
  return 0;
}
