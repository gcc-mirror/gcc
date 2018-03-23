// { dg-do compile { target c++11 } }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

#include <memory>

struct A
{
  virtual ~A() { }
};

struct B : A
{
};

void
test01()
{
  std::unique_ptr<B[]> up;
  up.reset(new A[3]);		// { dg-error "no matching function" }

  std::unique_ptr<A[]> up2;
  up2.reset(new B[3]);		// { dg-error "no matching function" }
}

struct A_pointer { operator A*() const { return nullptr; } };

void
test02()
{
  A_pointer p;
  // Disallow conversions from user-defined pointer-like types
  // for the array version
  std::unique_ptr<A[]> upA3;
  upA3.reset(p); // { dg-error "no matching function" }
  std::unique_ptr<const A[]> cA3;
  cA3.reset(p); // { dg-error "no matching function" }
  std::unique_ptr<volatile A[]> vA3;
  vA3.reset(p); // { dg-error "no matching function" }
  std::unique_ptr<const volatile A[]> cvA3;
  cvA3.reset(p); // { dg-error "no matching function" }
}
