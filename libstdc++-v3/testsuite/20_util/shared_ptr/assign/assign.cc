// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2006, 2007 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

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

struct reset_count_struct
{
  ~reset_count_struct()
  {
    A::ctor_count = 0;
    A::dtor_count = 0;
  }
};


// 20.6.6.2.3 shared_ptr assignment [util.smartptr.shared.assign]

// Assignment from shared_ptr<Y>
void
test01()
{
  reset_count_struct __attribute__((unused)) reset;
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> a;

  a = std::shared_ptr<A>(new A);
  VERIFY( a.get() != 0 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 0 );

  a = std::shared_ptr<A>();
  VERIFY( a.get() == 0 );
  VERIFY( A::ctor_count == 1 );
  VERIFY( A::dtor_count == 1 );
}

int 
main()
{
  test01();
  return 0;
}
