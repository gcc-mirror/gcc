// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

// 20.8.3 Class template reference_wrapper

// { dg-do compile { target c++11 } }

#include <functional>

struct X { };
X rval();
X&& rvalref();

void test01()
{
  std::ref(1);          // { dg-error "deleted" }
  std::cref(1);         // { dg-error "deleted" }
  std::ref( int() );    // { dg-error "deleted" }
  std::cref( int() );   // { dg-error "deleted" }
  std::ref(rval());     // { dg-error "deleted" }
  std::cref(rvalref()); // { dg-error "deleted" }
}

int main()
{
  test01();
}

// { dg-prune-output "declared here" }
