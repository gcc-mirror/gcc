// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

// PR libstdc++/69293

#include <tuple>
#include <memory>

using std::allocator;
using std::allocator_arg_t;
using std::uses_allocator;
using std::tuple;
using std::is_constructible;

struct X
{
  using allocator_type = allocator<int>;
};

using alloc_type = X::allocator_type;

static_assert(uses_allocator<X, alloc_type>{}, "");
static_assert(!is_constructible<X, allocator_arg_t, alloc_type>{}, "");
static_assert(!is_constructible<X, alloc_type>{}, "");

void
test01()
{
  alloc_type a;
  std::tuple<X> t(std::allocator_arg, a); // this is required to be ill-formed
  // { dg-error "static assertion failed" "" { target *-*-* } 89 }
}
