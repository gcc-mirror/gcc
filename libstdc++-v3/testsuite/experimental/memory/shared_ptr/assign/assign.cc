// { dg-do run { target c++14 } }

// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

struct reset_count_struct
{
  ~reset_count_struct()
  {
    A::ctor_count = 0;
    A::dtor_count = 0;
    B::ctor_count = 0;
    B::dtor_count = 0;
  }
};

// C++14 §20.8.2.2.3 shared_ptr assignment

void
test01()
{
  reset_count_struct __attribute__((unused)) reset;

  std::experimental::shared_ptr<A[5]> a;
  std::experimental::shared_ptr<A[]> a1;
  std::experimental::shared_ptr<B[5]> a2;

  a = std::experimental::shared_ptr<A[5]> ();
  VERIFY( a.get() == 0 );
  VERIFY( A::ctor_count == 0 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  a = std::experimental::shared_ptr<A[5]> (new A[5]);
  VERIFY( a.get() != 0 );
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  a1 = std::experimental::shared_ptr<A[5]> (new A[5]);
  VERIFY( a1.get() != 0 );
  VERIFY( A::ctor_count == 10 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  a2 = std::experimental::shared_ptr<B[5]> (new B[5]);
  VERIFY( a2.get() != 0 );
  VERIFY( A::ctor_count == 15 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 5 );
  VERIFY( B::dtor_count == 0 );
}

void
test02()
{
  std::experimental::shared_ptr<A[5]> p(new A[5]);
  std::experimental::shared_ptr<A[5]> p1;
  std::experimental::shared_ptr<A[]> p2;

  p1 = p;
  VERIFY( p.get() == p1.get() );

  p2 = p1;
  VERIFY( p1.get() == p2.get() );
}

int
main()
{
  test01();
  test02();
  return 0;
}
