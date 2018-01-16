// { dg-do run { target c++14 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

// 20.2.3 exchange [utility.exchange]

#include <utility>
#include <type_traits>
#include <testsuite_hooks.h>

void
test01()
{
  const unsigned val = 4;
  int i = 1;
  auto prev = std::exchange(i, val);
  static_assert( std::is_same<decltype(prev), int>::value, "return type" );
  VERIFY( i == 4 );
  VERIFY( prev == 1 );
  prev = std::exchange(i, 3);
  VERIFY( i == 3 );
  VERIFY( prev == 4 );
}

// Default construction from empty braces
void
test02()
{
  struct DefaultConstructible
  {
    DefaultConstructible(int i = 0) : value(i) { }
    int value;
  };

  DefaultConstructible x = 1;
  auto old = std::exchange(x, {});
  VERIFY( x.value == 0 );
  VERIFY( old.value == 1 );
}

int f(int) { return 0; }

double f(double) { return 0; }

// Deduce type of overloaded function
void
test03()
{
  int (*fp)(int);
  std::exchange(fp, &f);
  VERIFY( fp != nullptr );
}

void test04()
{
  struct From { };
  struct To {
    int value = 0;
    To() = default;
    To(const To&) = default;
    To(const From&) = delete;
    To& operator=(const From&) { value = 1; return *this; }
    To& operator=(From&&) { value = 2; return *this; }
  };

  To t;
  From f;

  auto prev = std::exchange(t, f);
  VERIFY( t.value == 1 );
  VERIFY( prev.value == 0 );

  prev = std::exchange(t, From{});
  VERIFY( t.value == 2 );
  VERIFY( prev.value == 1 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
