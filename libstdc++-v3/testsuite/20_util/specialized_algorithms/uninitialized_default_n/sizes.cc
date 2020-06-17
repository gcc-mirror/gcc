// Copyright (C) 2020 Free Software Foundation, Inc.
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
#include <algorithm>
#include <testsuite_hooks.h>

struct Value
{
  int value = 0x1234;
};

void
test01()
{
  alignas(Value) unsigned char buf[3 * sizeof(Value) + 1];
  std::fill(std::begin(buf), std::end(buf), 0xff);
  const auto p = reinterpret_cast<Value*>(buf);
  std::__uninitialized_default_n(p, 2.0001);
  VERIFY( p[0].value == 0x1234 );
  VERIFY( p[1].value == 0x1234 );
  VERIFY( p[2].value == 0x1234 );
  VERIFY( *std::prev(std::end(buf)) == 0xff );
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

  alignas(Value) unsigned char buf[4 * sizeof(Value) + 1];
  std::fill(std::begin(buf), std::end(buf), 0xff);
  const auto p = reinterpret_cast<Value*>(buf);
  Size n = {4};
  std::__uninitialized_default_n(p, n);
  VERIFY( p[0].value == 0x1234 );
  VERIFY( p[1].value == 0x1234 );
  VERIFY( p[2].value == 0x1234 );
  VERIFY( p[3].value == 0x1234 );
  VERIFY( *std::prev(std::end(buf)) == 0xff );
}

int
main()
{
  test01();
  test02();
}
