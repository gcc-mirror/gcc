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

struct A
{
  int i;
  virtual ~A() { }
};

struct B : A { };

// 8.2.1.2 shared_ptr observers [memory.smartptr.shared.obs]

// owner_before
void
test01()
{
  // test empty shared_ptrs compare equivalent
  std::experimental::shared_ptr<A[5]> p1;
  std::experimental::shared_ptr<B[5]> p2;
  VERIFY( !p1.owner_before(p2) && !p2.owner_before(p1) );
}

void
test02()
{
  std::experimental::shared_ptr<A[5]> a0;

  std::experimental::shared_ptr<A[5]> a1(new A[5]);
  VERIFY( a1.owner_before(a0) || a0.owner_before(a1) );
  VERIFY( !(a1.owner_before(a0) && a0.owner_before(a1)) );

  std::experimental::shared_ptr<B[5]> b1(new B[5]);
  VERIFY( a1.owner_before(b1) || b1.owner_before(a1) );
  VERIFY( !(a1.owner_before(b1) && b1.owner_before(a1)) );

  std::experimental::shared_ptr<A[5]> a2(a1);
  VERIFY( !a1.owner_before(a2) && !a2.owner_before(a1) );

  std::experimental::weak_ptr<A[5]> w1(a1);
  VERIFY( !a1.owner_before(w1) && !w1.owner_before(a1) );
}

void
test03()
{
  std::experimental::shared_ptr<A[5]> p1(new A[5]);
  std::experimental::shared_ptr<int> p2(p1, &p1[0].i);
  VERIFY( !p1.owner_before(p2) && !p2.owner_before(p1) );
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
