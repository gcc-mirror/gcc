// { dg-do run { target c++14 } }

// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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
struct D
{
  void operator()(A* p) { delete [] p; ++delete_count; }
  static long delete_count;
};
long D::delete_count = 0;

// C++14 §20.8.2.2.4

// reset
int
test01()
{
  A * const a = new A[5];
  std::experimental::shared_ptr<A[5]> p1(a);
  std::experimental::shared_ptr<A[5]> p2(p1);
  p1.reset();
  VERIFY( p1.get() == 0 );
  VERIFY( p2.get() == a );

  return 0;
}

int
test02()
{
  A * const a = new A[5];
  A * const b = new A[5];
  std::experimental::shared_ptr<A[5]> p1(a);
  std::experimental::shared_ptr<A[5]> p2(p1);
  p1.reset(b);
  VERIFY( p1.get() == b );
  VERIFY( p2.get() == a );

  return 0;
}

int
test03()
{
  {
    std::experimental::shared_ptr<A[5]> p1;
    p1.reset(new A[5], D());
  }
  VERIFY( D::delete_count == 1 );

  return 0;
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
