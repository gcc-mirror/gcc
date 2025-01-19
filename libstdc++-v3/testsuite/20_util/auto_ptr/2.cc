// Copyright (C) 2000-2025 Free Software Foundation, Inc.
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

// C++03 20.4.5 Template class auto_ptr [lib.auto.ptr]

// { dg-add-options using-deprecated }
// { dg-warning "auto_ptr. is deprecated" "" { target c++11 } 0 }

#include <memory>
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

// Construction from std::auto_ptr
int
test02()
{
  reset_count_struct __attribute__((unused)) reset;

  std::auto_ptr<A> A_from_A(new A);
  std::auto_ptr<B> B_from_B(new B);

  std::auto_ptr<A> A_from_ptr_A(A_from_A);
  std::auto_ptr<A> A_from_ptr_B(B_from_B);
  VERIFY( A_from_A.get() == 0 );
  VERIFY( B_from_B.get() == 0 );
  VERIFY( A_from_ptr_A.get() != 0 );
  VERIFY( A_from_ptr_B.get() != 0 );
  VERIFY( A::ctor_count == 2 );
  VERIFY( A::dtor_count == 0 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 0 );

  return 0;
}

int 
main()
{
  test02();
  return 0;
}
