// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <memory>
#include <testsuite_hooks.h>

void
test01()
{
  int i[3];
  auto j = std::__uninitialized_default_n(i, 2.0001);
  VERIFY( j == (i + 3) );
}

void
test02()
{
  // The standard only requires that n>0 and --n are valid expressions.
  struct Size
  {
    int value;

    void operator--() { --value; }

    int operator>(void*) { return value != 0; }
  };

  int i[5] = { 1, 2, 3, 4, 5 };
  Size n = {4};
  auto j = std::__uninitialized_default_n(i, n);
  VERIFY( j == (i + 4) );
  // i[0:3] are default-initialized so have indeterminate values.
  VERIFY( i[4] == 5 );
}

int
main()
{
  test01();
  test02();
}
