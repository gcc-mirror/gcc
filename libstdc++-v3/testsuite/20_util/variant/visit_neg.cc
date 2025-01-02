// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <variant>
#include <testsuite_hooks.h>

// { dg-error "same return type for all alternatives" "" { target *-*-* } 0 }
// { dg-prune-output "in 'constexpr' expansion" }

void
test01()
{
  {
    struct Visitor
    {
      double operator()(double) {return 0.02;}
      void operator()(int) {}
    };
    std::variant<int, double> v;
    std::visit(Visitor(), v);
  }
}

int
main()
{
  test01();
}
