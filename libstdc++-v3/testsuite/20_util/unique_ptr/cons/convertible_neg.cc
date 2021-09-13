// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

// 20.7.1.3 unique_ptr for array objects [unique.ptr.runtime]

// Conversion to non-array form of unique_ptr
void
test01()
{
  std::unique_ptr<A[]> array(new A[1]);
  std::unique_ptr<A> nonarray(std::move(array)); //{ dg-error "no matching function" }
  nonarray = std::move(array); //{ dg-error "no match" }
}

// { dg-prune-output "include" }
