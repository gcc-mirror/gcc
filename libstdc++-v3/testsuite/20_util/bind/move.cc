// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.h>

// PR libstdc++/45924

struct f
{
  f() : i(0) { }
  f(f&& r) : i(1) { r.i = -1; }
  f(const f&) = delete;
  int operator()() { return i; }
  int i;
};

void test01()
{
  auto b = std::bind(f());
  VERIFY( b() == 1 );
  auto bc(std::move(b));
  VERIFY( bc() == 1 );
  VERIFY( b() == -1 );
}

void test02()
{
  auto b = std::bind<int>(f());
  VERIFY( b() == 1 );
  auto bc(std::move(b));
  VERIFY( bc() == 1 );
  VERIFY( b() == -1 );
}

int main()
{
  test01();
  test02();
  return 0;
}

