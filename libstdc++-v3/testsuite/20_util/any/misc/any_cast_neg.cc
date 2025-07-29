// { dg-do compile { target c++17 } }

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

#include <any>

using std::any;
using std::any_cast;

void test01()
{
  const any y(1);
  any_cast<int&>(y); // { dg-error "here" }
  // { dg-error "Template argument must be constructible from a const value" "" { target { *-*-* } } 0 }
  // { dg-error "binding reference of type 'int&' to 'const int' discards qualifiers" "" { target { *-*-* } } 0 }
}

void test02()
{
  any y(1);
  any_cast<int&&>(y); // { dg-error "here" }
  // { dg-error "Template argument must be constructible from an lvalue" "" { target { *-*-* } } 0 }
  // { dg-error "cannot bind rvalue reference of type 'int&&' to lvalue of type 'int'" "" { target { *-*-* } } 0 }
}

void test03()
{
  any y(1);
  any_cast<int&>(std::move(y)); // { dg-error "here" }
  // { dg-error "Template argument must be constructible from an rvalue" "" { target { *-*-* } } 0 }
  // { dg-error "cannot bind non-const lvalue reference of type 'int&' to an rvalue of type 'int'" "" { target { *-*-* } } 0 }
}

// { dg-prune-output "invalid 'static_cast'" }
