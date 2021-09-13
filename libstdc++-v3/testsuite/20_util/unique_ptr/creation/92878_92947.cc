// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

void test01()
{
  auto x = std::make_unique<aggressive_aggregate>(1, 2);
  VERIFY(x->a == 1);
  VERIFY(x->b == 2);
  auto y = std::make_unique<aggressive_aggregate>(1);
  VERIFY(y->a == 1);
  VERIFY(y->b == 0);
  auto z = std::make_unique<aggressive_aggregate>();
  VERIFY(z->a == 0);
  VERIFY(z->b == 0);
}

int main()
{
  test01();
}
