// { dg-do run { xfail *-*-* } }
// { dg-options "-std=gnu++14" }

// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

#include <experimental/memory_resource>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using std::experimental::pmr::polymorphic_allocator;
using std::experimental::pmr::null_memory_resource;
using std::experimental::pmr::memory_resource;

void test01() {
  memory_resource* r = null_memory_resource();
  auto p = r->allocate(1);
}

int main() {
  test01();
}
