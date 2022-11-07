// Copyright (C) 2013-2022 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <testsuite_hooks.h>

void test01()
{
  using F = void();
  F* f = nullptr;
  std::function<F> x(f);
  VERIFY( !x ); // libstdc++/57465
}

void test02()
{
  struct X { };
  int (X::*mf)() = nullptr;
  std::function<int(X&)> f = mf;
  VERIFY( !f ); // libstdc++/69243

  int X::*mp = nullptr;
  f = mp;
  VERIFY( !f );
}

int main()
{
  test01();
  test02();
}
