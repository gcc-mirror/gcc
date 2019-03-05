// { dg-do run { target c++14 } }

// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

using std::experimental::propagate_const;

int main()
{
  constexpr propagate_const<int*> test1{};
  static_assert(!test1.get(), "");
  propagate_const<int*> test2; // wrapped pointer is not initialized
  propagate_const<int*> test3{};
  VERIFY(!test3.get());
}
