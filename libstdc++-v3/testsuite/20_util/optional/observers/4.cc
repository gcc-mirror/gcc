// { dg-do run { target c++17 }  }

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>

struct value_type
{
  int i;
};

void test01()
{
  std::optional<value_type> o { value_type { 51 } };
  value_type fallback { 3 };
  VERIFY( o.value_or(fallback).i == 51 );
  VERIFY( o.value_or(fallback).i == (*o).i );
}

void test02()
{
  std::optional<value_type> o;
  value_type fallback { 3 };
  VERIFY( o.value_or(fallback).i == 3 );
}

void test03()
{
  std::optional<value_type> o { value_type { 51 } };
  value_type fallback { 3 };
  VERIFY( std::move(o).value_or(fallback).i == 51 );
  VERIFY( o.has_value() );
  VERIFY( std::move(o).value_or(fallback).i == (*o).i );
}

void test04()
{
  std::optional<value_type> o;
  value_type fallback { 3 };
  VERIFY( std::move(o).value_or(fallback).i == 3 );
  VERIFY( !o.has_value() );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
