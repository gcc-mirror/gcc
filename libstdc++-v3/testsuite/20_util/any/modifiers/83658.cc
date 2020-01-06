// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <any>
#include <new>
#include <testsuite_hooks.h>

struct E : std::bad_alloc { };

struct X
{
    X() = default;
    X(std::initializer_list<int>) { }

    // Prevents small-object optimization:
    X(const X&) noexcept(false) { }

    static void* operator new(std::size_t) { throw E{}; }
    static void operator delete(void*, std::size_t) noexcept { }
};

void
test01()
{
  std::any a;
  try
  {
    a.emplace<X>();
    VERIFY(false);
  }
  catch (const E&)
  {
    VERIFY( !a.has_value() );
  }
}

void
test02()
{
  std::any a;
  try
  {
    a.emplace<X>(std::initializer_list<int>{});
    VERIFY(false);
  }
  catch (const E&)
  {
    VERIFY( !a.has_value() );
  }
}

int
main()
{
  test01();
  test02();
}
