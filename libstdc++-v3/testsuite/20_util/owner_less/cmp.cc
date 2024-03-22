// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

// 20.8.13.4 Template class owner_less [util.smartptr.ownerless]

#include <memory>
#include <algorithm>
#include <testsuite_hooks.h>

struct A { };

struct B { A a[2]; };

// 20.8.13.4 Template class owner_less [util.smartptr.ownerless]

int
test01()
{
  // test empty shared_ptrs compare equivalent
  std::owner_less<std::shared_ptr<A>> less;
  std::owner_less<std::weak_ptr<A>> wless;
  std::shared_ptr<A> p1;
  std::shared_ptr<A> p2;
  VERIFY( !less(p1, p2) && !less(p2, p1) );
  std::weak_ptr<A> p3;
  VERIFY( !less(p1, p3) && !less(p3, p1) );
  VERIFY( !wless(p1, p3) && !wless(p3, p1) );
  return 0;
}


// Construction from pointer
int
test02()
{
  std::owner_less<std::shared_ptr<A>> less;
  std::owner_less<std::weak_ptr<A>> wless;

  std::shared_ptr<A> empty;

  std::shared_ptr<A> a1(new A);
  VERIFY( less(empty, a1) || less(a1, empty) );

  std::shared_ptr<A> a2(new A);
  VERIFY( less(a1, a2) || less(a2, a1) );

  std::weak_ptr<A> w1(a1);
  VERIFY( !less(a1, w1) && !less(w1, a1) );

  std::weak_ptr<A> w2(a2);
  VERIFY( wless(w1, w2) || wless(w2, w1) );

  a1.reset();
  VERIFY( !less(empty, a1) && !less(a1, empty) );
  VERIFY( less(a1, w1) || less(w1, a1) );

  a2.reset();
  VERIFY( !less(a2, a1) && !less(a1, a2) );

  return 0;
}

// aliasing
int
test03()
{
  std::owner_less<std::shared_ptr<A>> less;
  std::owner_less<std::weak_ptr<A>> wless;

  std::shared_ptr<B> b(new B);
  std::shared_ptr<A> a0(b, &b->a[0]);
  std::shared_ptr<A> a1(b, &b->a[1]);
  // values are different but owners are equivalent:
  VERIFY( a0 < a1 && !less(a0, a1) && !less(a1, a0) );

  std::weak_ptr<A> w0(a0);
  std::weak_ptr<A> w1(a1);
  VERIFY( !wless(w0, w1) && !wless(w1, w0) );
  VERIFY( !less(a0, w1) && !less(w1, a0) );
  VERIFY( !wless(w0, a1) && !wless(a1, w0) );

  return 0;
}

// strict weak ordering
int
test04()
{
  std::owner_less<std::shared_ptr<A>> less;

  std::shared_ptr<A> a[3];
  a[0].reset(new A);
  a[1].reset(new A);
  a[2].reset(new A);
  std::sort(a, a+3, less);
  VERIFY( !less(a[0], a[0]) );
  VERIFY( less(a[0], a[1]) && !less(a[1], a[0]) );
  VERIFY( less(a[0], a[1]) && less(a[1], a[2]) && less(a[0], a[2]) );

  return 0;
}

int 
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
