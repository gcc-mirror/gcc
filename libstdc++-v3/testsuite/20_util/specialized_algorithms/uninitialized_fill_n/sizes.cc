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

// { dg-do run }

#include <memory>
#include <testsuite_hooks.h>

void
test01()
{
  int i[4] = { };
  // Floating-point n should work, but only if it's an integer value.
  std::uninitialized_fill_n(i, 3.0, 0xabcd);
  VERIFY( i[0] == 0xabcd );
  VERIFY( i[1] == 0xabcd );
  VERIFY( i[2] == 0xabcd );
  VERIFY( i[3] == 0 );
}

// The standard only requires that `if (n--)` is a valid expression.
struct Size
{
  int value;

  struct testable
  {
#if __cplusplus >= 201103L
    explicit
#endif
    operator bool() const { return nonzero; }

    bool nonzero;
  };

  testable operator--(int)
  {
    testable t = { value != 0 };
    --value;
    return t;
  }
};

void
test02()
{
  int i[5] = { };
  Size n = {4};
  std::uninitialized_fill_n(i, n, 0xdcba);
  VERIFY( i[0] == 0xdcba );
  VERIFY( i[1] == 0xdcba );
  VERIFY( i[2] == 0xdcba );
  VERIFY( i[3] == 0xdcba );
  VERIFY( i[4] == 0 );
}

int
main()
{
  test01();
  test02();
}
