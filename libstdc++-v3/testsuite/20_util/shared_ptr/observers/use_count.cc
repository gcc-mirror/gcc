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

struct A { };
struct B : A { };

// 20.6.6.2.5 shared_ptr observers [util.smartptr.shared.obs]

// use_count
int
test01()
{
  bool test __attribute__((unused)) = true;

  const std::shared_ptr<A> p1;
  VERIFY( p1.use_count() == 0 );
  const std::shared_ptr<A> p2(p1);
  VERIFY( p1.use_count() == 0 );

  return 0;
}

int
test02()
{
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> p1(new A);
  std::shared_ptr<A> p2(p1);
  p1.reset();
  VERIFY( p1.use_count() == 0 );
  VERIFY( p2.use_count() == 1 );

  return 0;
}

int
test03()
{
  bool test __attribute__((unused)) = true;

  std::shared_ptr<A> p1(new A);
  std::shared_ptr<A> p2(p1);
  p2.reset(new B);
  VERIFY( p1.use_count() == 1 );
  VERIFY( p2.use_count() == 1 );

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
