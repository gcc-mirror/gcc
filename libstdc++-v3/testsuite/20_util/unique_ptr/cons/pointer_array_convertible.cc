// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2008, 2009 Free Software Foundation
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
};

struct B : A
{
  virtual ~B() { }
};

// 20.4.5.1 unique_ptr constructors [unique.ptr.cons]

// Construction from pointer of derived type
void
test01()
{
  std::unique_ptr<B[]> B_from_A(new A[3]); //{ dg-error "invalid conversion from" }
}

// { dg-prune-output "include" }
