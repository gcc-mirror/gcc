// { dg-do run { target c++11 } }

// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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

// TR1 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <utility>
#include <testsuite_hooks.h>

struct A
{
  A() { ++ctor_count; }
  virtual ~A() { ++dtor_count; }
  static long ctor_count;
  static long dtor_count;
};
long A::ctor_count = 0;
long A::dtor_count = 0;

struct B : A
{
  B() { ++ctor_count; }
  virtual ~B() { ++dtor_count; }
  static long ctor_count;
  static long dtor_count;
};
long B::ctor_count = 0;
long B::dtor_count = 0;

struct D
{
  void operator()(B* p) const { delete p; ++delete_count; }
  static long delete_count;
};
long D::delete_count = 0;

struct reset_count_struct
{
  ~reset_count_struct()
  {
    A::ctor_count = 0;
    A::dtor_count = 0;
    B::ctor_count = 0;
    B::dtor_count = 0;
    D::delete_count = 0;
  }
};

// 20.6.6.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Rvalue construction
int test01()
{
  reset_count_struct __attribute__((unused)) reset;

  std::shared_ptr<A> a1;
  std::shared_ptr<A> a2(std::move(a1));
  VERIFY( a1.use_count() == 0 );
  VERIFY( a2.use_count() == 0 );
  VERIFY( A::ctor_count == 0 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int
test02()
{
  reset_count_struct __attribute__((unused)) reset;

  std::shared_ptr<A> a1(new A);
  std::shared_ptr<A> a2(std::move(a1));
  VERIFY( a1.use_count() == 0 );
  VERIFY( a2.use_count() == 1 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );

  return 0;
}

int
test03()
{
  reset_count_struct __attribute__((unused)) reset;

  std::shared_ptr<B> b(new B);
  std::shared_ptr<A> a(std::move(b));
  VERIFY( b.use_count() == 0 );
  VERIFY( a.use_count() == 1 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int
test04()
{
  reset_count_struct __attribute__((unused)) reset;

  std::shared_ptr<B> b(new B, D());
  std::shared_ptr<A> a(std::move(b));
  VERIFY( b.use_count() == 0 );
  VERIFY( a.use_count() == 1 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 0 );

  a = std::move(std::shared_ptr<A>());
  VERIFY( D::delete_count == 1 );
  VERIFY( B::dtor_count == 1 );

  return 0;
}

int
test05()
{
  reset_count_struct __attribute__((unused)) reset;

  std::shared_ptr<A> a(std::move(std::shared_ptr<A>(new A)));
  VERIFY( a.use_count() == 1 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );

  return 0;
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}
