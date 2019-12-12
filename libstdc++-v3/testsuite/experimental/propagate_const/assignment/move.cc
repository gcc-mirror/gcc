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
#include <utility>
#include <memory>

using std::experimental::propagate_const;
using std::unique_ptr;

int main()
{
  const int dummy{42};
  propagate_const<const int*> test1;
  test1 = std::move(&dummy);
  test1 = &dummy;
  VERIFY(test1.get() == &dummy);
  propagate_const<const int*> test2;
  test2 = std::move(test1);
  VERIFY(test1.get() == &dummy);
  VERIFY(test2.get() == &dummy);
  propagate_const<const int*> test3;
  test3 = std::move(test2);
  VERIFY(test1.get() == &dummy);
  VERIFY(test2.get() == &dummy);
  VERIFY(test3.get() == &dummy);
  propagate_const<unique_ptr<const int>> test4;
  test4 = std::make_unique<int>(666);
}
