// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

// 20.8.2.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>

void test01()
{
  std::shared_ptr<void> p((void*)nullptr);   // { dg-error "here" }
  // { dg-error "incomplete" "" { target *-*-* } 0 }
}

using std::shared_ptr;
using std::is_constructible;
static_assert(!is_constructible<shared_ptr<void>, const void*>::value, "");

// { dg-prune-output "invalid application of 'sizeof' to a void type" }
