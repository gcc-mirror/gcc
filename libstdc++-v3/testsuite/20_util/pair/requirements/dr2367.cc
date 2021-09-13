// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

#include <utility>
#include <type_traits>

// DR 2367, pair and tuple are not correctly implemented for is_constructible with no args
void test_default_constructible()
{
  struct X
  {
    X() = delete;
  };

  typedef std::pair<int, X> P;
  static_assert(!std::is_constructible<P>::value, "");
  static_assert(!std::is_default_constructible<P>::value, "");
}

int main()
{
  test_default_constructible();
  return 0;
}
