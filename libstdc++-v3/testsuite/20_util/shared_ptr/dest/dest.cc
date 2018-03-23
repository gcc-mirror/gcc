// { dg-do run { target c++11 } }

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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
  A() { ++ctor_count; }
  ~A() { ++dtor_count; }
  static long ctor_count;
  static long dtor_count;
};
long A::ctor_count = 0;
long A::dtor_count = 0;

struct B : A
{
  B() { ++ctor_count; }
  ~B() { ++dtor_count; }
  static long ctor_count;
  static long dtor_count;
};
long B::ctor_count = 0;
long B::dtor_count = 0;

struct D
{
  void operator()(const B* p) { delete p; ++delete_count; }
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


// 20.6.6.2.2 shared_ptr destructor [util.smartptr.shared.dest]

// empty shared_ptr
int
test01()
{
  reset_count_struct __attribute__((unused)) reset;

  {
    std::shared_ptr<A> a;
  }
  VERIFY( A::ctor_count == 0 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );
  VERIFY( D::delete_count == 0 );

  return 0;
}

// shared ownership
int
test02()
{
  reset_count_struct __attribute__((unused)) reset;

  std::shared_ptr<A> a;
  {
    a = std::shared_ptr<A>(new B, D());
  }
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 0 );
  VERIFY( D::delete_count == 0 );

  return 0;
}

// exclusive ownership
int
test03()
{
  reset_count_struct __attribute__((unused)) reset;

  {
    std::shared_ptr<A> a1(new B);
    std::shared_ptr<A> a2(new B, D());
  }
  VERIFY( A::ctor_count == 2 );
  VERIFY( A::dtor_count == 2 );
  VERIFY( B::ctor_count == 2 );
  VERIFY( B::dtor_count == 2 );
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
