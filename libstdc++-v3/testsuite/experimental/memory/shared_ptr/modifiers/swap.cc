// { dg-do run { target c++14 } }

// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

// 8.2.1 Class template shared_ptr [memory.smartptr.shared]

#include <experimental/memory>
#include <testsuite_hooks.h>

struct A { };

// C++14 §20.8.2.2.4

// swap
int
test01()
{
  A * const a1 = new A[5];
  A * const a2 = new A[5];
  std::experimental::shared_ptr<A[5]> p1(a1);
  std::experimental::shared_ptr<A[5]> p2(a2);
  p1.swap(p2);
  VERIFY( p1.get() == a2 );
  VERIFY( p2.get() == a1 );

  return 0;
}



int
main()
{
  test01();
  return 0;
}
