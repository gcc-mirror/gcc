// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/memory>
#include <testsuite_hooks.h>

struct A : std::enable_shared_from_this<A> { };
struct B : std::experimental::enable_shared_from_this<B> { };
struct C : A, B { };

void
test01()
{
  // This should not fail to compile due to ambiguous base classes:
  std::experimental::shared_ptr<C> p(new C);

  // And both base classes should have been enabled:
  std::shared_ptr<A> pa = p->A::shared_from_this();
  VERIFY( pa != nullptr );
  // Can't compare pa and p because they're different types

  std::experimental::shared_ptr<B> pb = p->B::shared_from_this();
  VERIFY( pb != nullptr );
  VERIFY( pb == p );
}

int
main()
{
  test01();
}
