// { dg-do run { target c++14 } }

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
  void operator()(const B* p) { delete [] p; ++delete_count; }
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

// 20.8.2.2.2 shared_ptr destructor

int
test01()
{
  reset_count_struct __attribute__((unused)) reset;

  {
    std::experimental::shared_ptr<A[5]> a;
  }
  VERIFY( A::ctor_count == 0 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 0 );
  VERIFY( B::dtor_count == 0 );
  VERIFY( D::delete_count == 0 );

  return 0;
}

int
test02()
{
  reset_count_struct __attribute__((unused)) reset;

  {
    std::experimental::shared_ptr<B[5]> a;
    a = std::experimental::shared_ptr<B[5]>(new B[5], D());
  }
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 5 );
  VERIFY( B::ctor_count == 5 );
  VERIFY( B::dtor_count == 5 );
  VERIFY( D::delete_count == 1 );

  return 0;
}

int
test03()
{
  reset_count_struct __attribute__((unused)) reset;

  {
    std::experimental::shared_ptr<B[]> a;
    a = std::experimental::shared_ptr<B[5]>(new B[5], D());
  }
  VERIFY( A::ctor_count == 5 );
  VERIFY( A::dtor_count == 5 );
  VERIFY( B::ctor_count == 5 );
  VERIFY( B::dtor_count == 5 );
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
