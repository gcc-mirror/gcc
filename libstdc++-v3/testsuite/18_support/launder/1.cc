// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <new>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void
test01()
{
  struct S {
    int i;
  } s;
  auto p = std::launder( &s.i );
  VERIFY( p == &s.i );
}


void
test02()
{
  // C++17 1.8 [intro.object]
  struct X { const int n; };
  union U { X x; float f; };

  U u = {{ 1 }};
  u.f = 5.f; // OK, creates new subobject of u (9.3)
  X *p = new (&u.x) X {2}; // OK, creates new subobject of u
  VERIFY(p->n == 2); // OK
  VERIFY(*std::launder(&u.x.n) == 2); // OK
  // assert(u.x.n == 2); undefined behavior, u.x does not name new subobject
}

int
main()
{
  test01();
  test02();
}
