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

void deleter(A* p) { delete [] p; }

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

// 8.2.1.1 shared_ptr constructors [memory.smartptr.shared.const]

// Copy construction

int
test01()
{
  reset_count_struct __attribute__((unused)) reset;

  std::experimental::shared_ptr<A[5]> a1;
  std::experimental::shared_ptr<A[5]> a2(a1);
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

  std::experimental::shared_ptr<A[5]> a1(new A[5]);
  std::experimental::shared_ptr<A[5]> a2(a1);
  VERIFY( a2.use_count() == 2 );
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int
test03()
{
  reset_count_struct __attribute__((unused)) reset;

  std::experimental::shared_ptr<A[5]> a1(new A[5], &deleter);
  std::experimental::shared_ptr<A[5]> a2(a1);
  VERIFY( a2.use_count() == 2 );
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int
test04()
{
  reset_count_struct __attribute__((unused)) reset;

  std::experimental::shared_ptr<A[5]> a1(std::experimental::shared_ptr<A[5]>
                                          (new A[5]));
  VERIFY( a1.use_count() == 1 );
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int
test05()
{
  reset_count_struct __attribute__((unused)) reset;

  std::experimental::shared_ptr<A[5]> a1(new A[5]);
  std::experimental::shared_ptr<A[]> a2(a1);

  VERIFY( a2.use_count() == 2 );
  VERIFY( a2.get() == a1.get() );
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int
test06()
{
  reset_count_struct __attribute__((unused)) reset;

  std::experimental::shared_ptr<B> a1(new B);
  std::experimental::shared_ptr<A> a2(a1);

  VERIFY( a2.use_count() == 2 );
  VERIFY( a2.get() == a1.get() );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 0 );

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
  test06();
  return 0;
}
