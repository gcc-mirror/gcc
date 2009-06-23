// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2006, 2007, 2009 Free Software Foundation
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
  virtual ~A() { }
};

struct B : A
{
};

// 20.6.6.2.6 shared_ptr comparison [util.smartptr.shared.cmp]

int
test01()
{
  bool test __attribute__((unused)) = true;

  // test empty shared_ptrs compare equivalent
  std::shared_ptr<A> p1;
  std::shared_ptr<B> p2;
  VERIFY( p1 == p2 );
  VERIFY( !(p1 != p2) );
  VERIFY( !(p1 < p2) && !(p2 < p1) );
  return 0;
}


// Construction from pointer
int
test02()
{
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> A_default;

  std::shared_ptr<A> A_from_A(new A);
  VERIFY( A_default != A_from_A );
  VERIFY( !(A_default == A_from_A) );
  VERIFY( (A_default < A_from_A) || (A_from_A < A_default) );

  std::shared_ptr<B> B_from_B(new B);
  VERIFY( B_from_B != A_from_A );
  VERIFY( !(B_from_B == A_from_A) );
  VERIFY( (B_from_B < A_from_A) || (A_from_A < B_from_B) );

  A_from_A.reset();
  VERIFY( A_default == A_from_A );
  VERIFY( !(A_default != A_from_A) );
  VERIFY( !(A_default < A_from_A) && !(A_from_A < A_default) );

  B_from_B.reset();
  VERIFY( B_from_B == A_from_A );
  VERIFY( !(B_from_B != A_from_A) );
  VERIFY( !(B_from_B < A_from_A) && !(A_from_A < B_from_B) );

  return 0;
}

int
test03()
{
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> p1;

  // check other operators are defined
  VERIFY( p1 <= p1 );
  VERIFY( p1 >= p1 );
  VERIFY( !(p1 > p1) );

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
