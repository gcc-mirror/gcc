// { dg-do run { target c++11 } }

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

void deletefunc(A* p) { delete p; }

// 20.6.6.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Aliasing constructors

int test01()
{
  bool test = true;

  std::shared_ptr<A> a;
  std::shared_ptr<bool> b1(a, &test);
  VERIFY( b1.use_count() == 0 );
  VERIFY( a.get() == 0 );
  VERIFY( b1.get() == &test );

  std::shared_ptr<bool> b2(b1);
  VERIFY( b2.use_count() == 0 );
  VERIFY( b1.get() == b2.get() );

  return 0;
}

int
test02()
{
  std::shared_ptr<A> a(new A);
  std::shared_ptr<int> i1(a, &a->i);
  VERIFY( i1.use_count() == 2 );

  std::shared_ptr<int> i2(i1);
  VERIFY( i2.use_count() == 3 );
  VERIFY( i2.get() == &a->i );

  return 0;
}

int
test03()
{
  std::shared_ptr<B> b(new B);
  std::shared_ptr<A> a1(b, b.get());
  std::shared_ptr<A> a2(b, &b->a);
  VERIFY( a2.use_count() == 3 );
  VERIFY( a1 == b );
  VERIFY( a2 != b );
  VERIFY( a1.get() != a2.get() );

  std::shared_ptr<A> a3(a1);
  VERIFY( a3 == b );

  a3 = a2;
  VERIFY( a3.get() == &b->a );

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
