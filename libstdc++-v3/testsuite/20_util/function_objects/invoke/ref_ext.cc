// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <functional>

struct A {
  void foo(int) { }
};

void
test01()
{
  // PR libstdc++/59768
  A a;
  auto ref = std::ref(a);
  std::__invoke(&A::foo, ref, 100);		// lvalue
  std::__invoke(&A::foo, std::move(ref), 100);	// rvalue
  const auto refc = std::ref(a);
  std::__invoke(&A::foo, refc, 100);		// const lvalue
  std::__invoke(&A::foo, std::move(refc), 100);	// const rvalue
}

struct B {
  int bar = 0;
};

void
test02()
{
  B b;
  // Invocation through a reference_wrapper means the object is an lvalue.

  int* ptr [[gnu::unused]];
  auto ref = std::ref(b);
  ptr = &std::__invoke(&B::bar, ref);
  ptr = &std::__invoke(&B::bar, std::move(ref));

  const int* cptr [[gnu::unused]];
  auto cref = std::cref(b);
  cptr = &std::__invoke(&B::bar, cref);
  cptr = &std::__invoke(&B::bar, std::move(cref));
}
