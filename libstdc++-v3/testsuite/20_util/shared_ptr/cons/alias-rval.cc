// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target hosted }

// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// Template class shared_ptr [util.smartptr.shared]

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

// 20.6.6.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Aliasing constructors

void
test01()
{
  bool test = true;

  std::shared_ptr<A> a;
  std::shared_ptr<bool> b1(std::move(a), &test);
  VERIFY( b1.use_count() == 0 );
  VERIFY( b1.get() == &test );
  VERIFY( a.use_count() == 0 );
  VERIFY( a == nullptr );

  std::shared_ptr<bool> b2(b1);
  VERIFY( b2.use_count() == 0 );
  VERIFY( b1 == b2 );
}

void
test02()
{
  A* ptr = new A;
  ptr->i = 100;
  std::shared_ptr<A> a(ptr);
  std::shared_ptr<int> i1(std::move(a), &a->i);
  VERIFY( i1.use_count() == 1 );
  VERIFY( *i1 == 100 );
  VERIFY( a.use_count() == 0 );
  VERIFY( a == nullptr );

  std::shared_ptr<int> i2(i1);
  VERIFY( i2.use_count() == 2 );
  VERIFY( i2.get() == &ptr->i );
}

void
test03()
{
  std::shared_ptr<B> b1(new B);
  b1->i = 100;
  b1->a.i = 200;
  std::shared_ptr<B> b2(b1);
  std::shared_ptr<A> a1(std::move(b1), b1.get());
  std::shared_ptr<A> a2(std::move(b2), &b2->a);
  VERIFY( a1.use_count() == 2 );
  VERIFY( a2.use_count() == 2 );
  VERIFY( a1->i == 100 );
  VERIFY( a2->i == 200 );
  VERIFY( b1.use_count() == 0 );
  VERIFY( b2.use_count() == 0 );
  VERIFY( b1 == nullptr );
  VERIFY( b2 == nullptr );
}

int
main()
{
  test01();
  test02();
  test03();
}
