// Copyright (C) 2000-2013 Free Software Foundation, Inc.
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

// 20.4.5 Template class auto_ptr [lib.auto.ptr]

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


// Destruction
int
test04()
{
  reset_count_struct __attribute__((unused)) reset;
  bool test __attribute__((unused)) = true;

  {/*lifetine scope*/
    std::auto_ptr<A> A_from_A(new A);
    std::auto_ptr<A> A_from_B(new B);
    std::auto_ptr<B> B_from_B(new B);
  }/*destructors called here*/

  VERIFY( A::ctor_count == 3 );
  VERIFY( A::dtor_count == 3 );
  VERIFY( B::ctor_count == 2 );
  VERIFY( B::dtor_count == 2 );

  return 0;
}

int 
main()
{
  test04();
  return 0;
}
