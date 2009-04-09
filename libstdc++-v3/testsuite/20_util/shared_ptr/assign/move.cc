// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007, 2009 Free Software Foundation
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


// 20.6.6.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Rvalue assignment from shared_ptr
void
test01()
{
  reset_count_struct __attribute__((unused)) reset;
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> a1;
  std::shared_ptr<A> a2(new A);

  a1 = std::move(a2);
  VERIFY( a1.get() != 0 );
  VERIFY( a2.get() == 0 );
  VERIFY( a1.use_count() == 1 );
  VERIFY( a2.use_count() == 0 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );

  a1 = std::move(std::shared_ptr<A>());
  VERIFY( a1.get() == 0 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 1 );
}

// Rvalue assignment from shared_ptr<Y>
void
test02()
{
  reset_count_struct __attribute__((unused)) reset;
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> a;
  std::shared_ptr<B> b(new B);

  a = std::move(b);
  VERIFY( a.get() != 0 );
  VERIFY( b.get() == 0 );
  VERIFY( a.use_count() == 1 );
  VERIFY( b.use_count() == 0 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 0 );

  a = std::move(std::shared_ptr<A>());
  VERIFY( a.get() == 0 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 1 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 1 );
}

int 
main()
{
  test01();
  test02();
  return 0;
}
