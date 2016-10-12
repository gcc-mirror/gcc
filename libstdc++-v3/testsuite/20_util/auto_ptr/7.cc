// Copyright (C) 2000-2016 Free Software Foundation, Inc.
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

// { dg-options "-std=c++98" }

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

// 20.4.5.3 auto_ptr conversions [lib.auto.ptr.conv]

// Parameters and return values
template <typename T>
static std::auto_ptr<T> source()
{
  return std::auto_ptr<T>(new T);
}

template <typename T>
static void drain(std::auto_ptr<T>)
{}

int
test07()
{
  reset_count_struct __attribute__((unused)) reset;

  drain(source<A>());
  // The resolution of core issue 84, now a DR, breaks this call.
  // drain<A>(source<B>());
  drain(source<B>());
  VERIFY( A::ctor_count == 2 );
  VERIFY( A::dtor_count == 2 );
  VERIFY( B::ctor_count == 1 );
  VERIFY( B::dtor_count == 1 );
  return 0;
}

int 
main()
{
  test07();
  return 0;
}
