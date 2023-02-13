// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

// 20.8.13.3 Template class weak_ptr [util.smartptr.weak]

#include <memory>
#include <testsuite_hooks.h>

struct A { };
struct B { };

// 20.6.6.3.5 weak_ptr observers [util.smartptr.weak.obs]

void
test01()
{
  // test empty weak_ptrs compare equivalent
  std::weak_ptr<A> p1;
  std::weak_ptr<B> p2;
  VERIFY( !p1.owner_before(p2) && !p2.owner_before(p1) );

  std::shared_ptr<B> p3;
  VERIFY( !p1.owner_before(p3) && !p3.owner_before(p1) );

  static_assert( noexcept(p1.owner_before(p1)), "" );
  static_assert( noexcept(p1.owner_before(p2)), "" );
  static_assert( noexcept(p1.owner_before(p3)), "" );
  static_assert( noexcept(p2.owner_before(p1)), "" );
}


void
test02()
{
  std::shared_ptr<A> a0;
  std::weak_ptr<A> w0(a0);

  std::shared_ptr<A> a1(new A);
  std::weak_ptr<A> w1(a1);
  VERIFY( !a1.owner_before(w1) && !w1.owner_before(a1) );

  VERIFY( w1.owner_before(w0) || w0.owner_before(w1) );
  VERIFY( !(w1.owner_before(w0) && w0.owner_before(w1)) );

  VERIFY( w1.owner_before(a0) || a0.owner_before(w1) );
  VERIFY( !(w1.owner_before(a0) && a0.owner_before(w1)) );

  std::shared_ptr<B> b1(new B);
  VERIFY( w1.owner_before(b1) || b1.owner_before(w1) );
}

int 
main()
{
  test01();
  test02();
  return 0;
}
