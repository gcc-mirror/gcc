// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009 Free Software Foundation
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

// 20.8.13.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

struct A { };

namespace std
{
  template<>
    struct less<A*> : binary_function<A*,A*,bool>
    {
      static int count;
      bool operator()(A* l, A* r) { ++count; return l < r; }
    };
  int less<A*>::count = 0;
}

// 20.8.13.2.7 shared_ptr comparison [util.smartptr.shared.cmp]


int
test01()
{
  std::less<std::shared_ptr<A>> less;
  // test empty shared_ptrs compare equivalent
  std::shared_ptr<A> p1;
  std::shared_ptr<A> p2;
  VERIFY( !less(p1, p2) && !less(p2, p1) );
  VERIFY( std::less<A*>::count == 2 );
  return 0;
}


// Construction from pointer
int
test02()
{
  std::less<std::shared_ptr<A>> less;

  std::shared_ptr<A> empty;
  std::shared_ptr<A> p1(new A);
  std::shared_ptr<A> p2(new A);

  VERIFY( less(p1, p2) || less(p2, p1) );
  VERIFY( !(less(p1, p2) && less(p2, p1)) );

  p1.reset();
  VERIFY( !less(p1, empty) && !less(empty, p1) );

  p2.reset();
  VERIFY( !less(p1, p2) && !less(p2, p1) );

  return 0;
}

// Aliasing
int
test03()
{
  std::less<std::shared_ptr<A>> less;

  A a;
  std::shared_ptr<A> p1(new A);
  std::shared_ptr<A> p2(p1, &a);
  VERIFY( less(p1, p2) || less(p2, p1) );

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
