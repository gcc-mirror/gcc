// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++1z" }
// { dg-do compile }

#include <functional>

struct A {
  void foo(int) { }
};

void
test01()
{
  A a;
  auto ref = std::ref(a);
  std::invoke(&A::foo, ref, 100);		// lvalue
  std::invoke(&A::foo, std::move(ref), 100);	// rvalue
  const auto refc = std::ref(a);
  std::invoke(&A::foo, refc, 100);		// const lvalue
  std::invoke(&A::foo, std::move(refc), 100);	// const rvalue
}
