// { dg-do run { target c++14 } }

// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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
  A() : i() { }
  virtual ~A() { }
  int i;
};

struct B : A
{
  B() : A(), a() { }
  virtual ~B() { }
  A a;
};

// 8.2.1.1 shared_ptr constructors [memory.smartptr.shared.const]

// Aliasing constructors

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::experimental::shared_ptr<A[5]> a;
  std::experimental::shared_ptr<bool> b1(a, &test);
  VERIFY( b1.use_count() == 0 );
  VERIFY( a.get() == 0 );
  VERIFY( b1.get() == &test );

  std::experimental::shared_ptr<bool> b2(b1);
  VERIFY( b2.use_count() == 0 );
  VERIFY( b1.get() == b2.get() );
}

void
test02()
{
  bool test __attribute__((unused)) = true;

  std::experimental::shared_ptr<A[5]> a(new A[5]);
  std::experimental::shared_ptr<int> i1(a, &a[0].i);
  VERIFY( i1.use_count() == 2 );

  std::experimental::shared_ptr<int> i2(i1);
  VERIFY( i2.use_count() == 3 );
  VERIFY( i2.get() == &a[0].i );
}

void
test03()
{
  bool test __attribute__((unused)) = true;

  std::experimental::shared_ptr<B> b(new B);
  std::experimental::shared_ptr<A> a1(b, b.get());
  std::experimental::shared_ptr<A> a2(b, &b->a);
  VERIFY( a2.use_count() == 3 );
  VERIFY( a1 == b );
  VERIFY( a2 != b );
  VERIFY( a1.get() != a2.get() );

  std::experimental::shared_ptr<A> a3(a1);
  VERIFY( a3 == b );

  a3 = a2;
  VERIFY( a3.get() == &b->a );
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
