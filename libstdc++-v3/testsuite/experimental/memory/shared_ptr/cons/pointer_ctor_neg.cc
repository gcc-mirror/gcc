// { dg-do compile { target c++14 } }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// 8.2.1 Class template shared_ptr [memory.smartptr.shared]

#include <experimental/memory>

struct A { };
struct B : A { };
struct C { };
struct D { void operator()(B* p) const { delete[] p; } };

// 8.2.1.1 shared_ptr constructors [memory.smartptr.shared.const]

// Construction from pointer
void
test01()
{
  C * const c = nullptr;
  std::experimental::shared_ptr<A> p(c); // { dg-error "no match" }
}

void
test02()
{
  B * const b = nullptr;
  std::experimental::shared_ptr<A[5]> p(b); // { dg-error "no match" }
}

void
test03()
{
  B * const b = nullptr;
  std::experimental::shared_ptr<A[]> p(b); // { dg-error "no match" }
}
