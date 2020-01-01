// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

// 20.7.1 Class template unique_ptr [unique.ptr]

#include <memory>

struct A { virtual ~A() = default; };

struct B : A { };

// Construction from objects with different cv-qualification

void
test01()
{
  std::unique_ptr<const A> cA;
  cA.reset(new A);
  std::unique_ptr<volatile A> vA;
  vA.reset(new A);
  std::unique_ptr<const volatile A> cvA;
  cvA.reset(new A);
}

void
test02()
{
  std::unique_ptr<const A> cB;
  cB.reset(new B);
  std::unique_ptr<volatile A> vB;
  vB.reset(new B);
  std::unique_ptr<const volatile A> cvB;
  cvB.reset(new B);
}

void
test03()
{
  std::unique_ptr<const A[]> cA;
  cA.reset(new A[1]);
  std::unique_ptr<volatile A[]> vA;
  vA.reset(new A[1]);
  std::unique_ptr<const volatile A[]> cvA;
  cvA.reset(new A[1]);
}

struct A_pointer { operator A*() const { return nullptr; } };

void
test07()
{
  A_pointer p;
  // Allow conversions from user-defined pointer-like types
  // for the single-object version
  std::unique_ptr<A> upA;
  upA.reset(p);
  std::unique_ptr<const A> cA;
  cA.reset(p);
  std::unique_ptr<volatile A> vA;
  vA.reset(p);
  std::unique_ptr<const volatile A> cvA;
  cvA.reset(p);
  // Allow conversions from user-defined pointer-like types
  // for the array version when the type is converted explicitly
  std::unique_ptr<A[]> upA2;
  upA2.reset((A*)p);
  std::unique_ptr<const A[]> cA2;
  cA2.reset((A*)p);
  std::unique_ptr<volatile A[]> vA2;
  vA2.reset((A*)p);
  std::unique_ptr<const volatile A[]> cvA2;
  cvA2.reset((A*)p);
}
