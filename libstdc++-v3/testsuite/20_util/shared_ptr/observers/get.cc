// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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
  A() : i() {}
  int i;
};

// 20.6.6.2.5 shared_ptr observers [util.smartptr.shared.obs]

// get
void
test01()
{
  A * const a = new A;
  const std::shared_ptr<A> p(a);
  VERIFY( p.get() == a );
  static_assert( noexcept(p.get()), "non-throwing" );
}

// operator*
void
test02()
{
  A * const a = new A;
  const std::shared_ptr<A> p(a);
  VERIFY( &*p == a );
  static_assert( noexcept(*p), "non-throwing" );
}

// operator->
void
test03()
{
  A * const a = new A;
  const std::shared_ptr<A> p(a);
  VERIFY( &p->i == &a->i );
  static_assert( noexcept(p->i), "non-throwing" );
}

void
test04()
{
#if !(defined _GLIBCXX_DEBUG && defined _GLIBCXX_DEBUG_PEDANTIC)
  std::shared_ptr<int> p;
  auto np = p.operator->();
  VERIFY( np == nullptr );
#endif
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
