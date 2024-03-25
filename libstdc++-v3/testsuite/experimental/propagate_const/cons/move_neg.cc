// { dg-do compile { target c++14 } }

// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

#include <experimental/propagate_const>
#include <testsuite_hooks.h>
#include <utility>
#include <memory>

using std::experimental::propagate_const;
using std::unique_ptr;

int main()
{
  const int dummy{42};
  propagate_const<const int*> test1{&dummy};
  propagate_const<int*> test2{&dummy}; // { dg-error "no matching function" }
  propagate_const<int*> test3{std::move(test1)}; // { dg-error "no matching function" }
  propagate_const<unique_ptr<const int>> test4 = &dummy; // { dg-error "conversion" }
  propagate_const<unique_ptr<const int>> test5 = std::move(test1); // { dg-error "conversion" }
}
// { dg-prune-output "no type" }
