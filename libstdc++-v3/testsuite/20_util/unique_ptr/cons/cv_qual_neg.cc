// { dg-do compile { target c++11 } }

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

// 20.7.1 Class template unique_ptr [unique.ptr]

#include <memory>

struct A { virtual ~A() = default; };

struct B : A { };

// Construction from objects with different cv-qualification

struct A_pointer { operator A*() const { return nullptr; } };

void
test07()
{
  A_pointer p;
  // Disallow conversions from user-defined pointer-like types
  // for the array version
  std::unique_ptr<A[]> upA3(p); // { dg-error "no matching function" }
  std::unique_ptr<const A[]> cA3(p); // { dg-error "no matching function" }
  std::unique_ptr<volatile A[]> vA3(p); // { dg-error "no matching function" }
  std::unique_ptr<const volatile A[]> cvA3(p); // { dg-error "no matching function" }
  // { dg-error "no type" "" { target *-*-* } 473 }
}

template<typename T>
struct deleter
{
  deleter() = default;
  template<typename U>
    deleter(const deleter<U>) { }
  typedef T pointer;
  void operator()(T) const { }
};

void
test08()
{
  // Disallow conversions from non-assignable deleter
  std::unique_ptr<A[], deleter<A_pointer>> p;
  std::unique_ptr<A[], deleter<A*>> upA(std::move(p)); // { dg-error "no matching function" }
}

void
test011()
{
  // Disallow conversions between different array types.
  std::unique_ptr<B[]> upB;

  std::unique_ptr<const A[]> cA(std::move(upB));  // { dg-error "no matching function" }
  std::unique_ptr<volatile A[]> vA(std::move(upB)); // { dg-error "no matching function" }
  std::unique_ptr<const volatile A[]> cvA(std::move(upB)); // { dg-error "no matching function" }
}
