// { dg-options "-std=gnu++14" }
// { dg-do compile }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <experimental/optional>
#include <testsuite_hooks.h>

#include <string>
#include <memory>

int main()
{
  {
    struct X
    {
      explicit X(int) {}
    };
    std::experimental::optional<X> ox{42};
    std::experimental::optional<X> ox2 = 42; // { dg-error "conversion" }
    std::experimental::optional<std::unique_ptr<int>> oup{new int};
    std::experimental::optional<std::unique_ptr<int>> oup2 = new int;  // { dg-error "conversion" }
  }
}
