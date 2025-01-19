// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

// Assignment from objects with different cv-qualification

void
test01()
{
  std::unique_ptr<A> upA;

  std::unique_ptr<const A> cA;
  cA = std::move(upA);
  std::unique_ptr<volatile A> vA;
  vA = std::move(upA);
  std::unique_ptr<const volatile A> cvA;
  cvA = std::move(upA);
}

void
test02()
{
  std::unique_ptr<B> upB;

  std::unique_ptr<const A> cA;
  cA = std::move(upB);
  std::unique_ptr<volatile A> vA;
  vA = std::move(upB);
  std::unique_ptr<const volatile A> cvA;
  cvA = std::move(upB);
}

void
test03()
{
  std::unique_ptr<A[]> upA;

  std::unique_ptr<const A[]> cA;
  cA = std::move(upA);
  std::unique_ptr<volatile A[]> vA;
  vA = std::move(upA);
  std::unique_ptr<const volatile A[]> cvA;
  cvA = std::move(upA);
}
