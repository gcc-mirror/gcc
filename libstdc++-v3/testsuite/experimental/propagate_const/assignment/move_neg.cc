// { dg-do compile { target c++14 } }

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
  propagate_const<unique_ptr<const int>> test5;
  test5 = new int{666}; // { dg-error "no match" }
  const int* dummy2 = new int{666};
  propagate_const<unique_ptr<const int>> test6;
  test6 = dummy2; // { dg-error "no match" }
}
// { dg-prune-output "no type" }
