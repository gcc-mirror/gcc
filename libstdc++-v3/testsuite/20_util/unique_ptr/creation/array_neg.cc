// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

// 20.9.1.4 unique_ptr creation [unique.ptr.create]

#include <memory>
#include <testsuite_hooks.h>

struct A { };

auto p1 = std::make_unique<A[]>();      // { dg-error "no matching function" }
auto p2 = std::make_unique<A[]>(1, 2);  // { dg-error "no matching function" }
auto p3 = std::make_unique<A[1]>();     // { dg-error "deleted" }
auto p4 = std::make_unique<A[1]>(1);    // { dg-error "deleted" }

// { dg-prune-output "declared here" }
// { dg-prune-output "no type named" }
