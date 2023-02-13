// { dg-options "-fno-rtti" }
// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2009-2023 Free Software Foundation, Inc.
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

// 20.8.15.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

// libstdc++/42019

class A {};

struct B {
  explicit B(int i) : i(i) { }
  int i;
};

void test01()
{
  std::shared_ptr<A> spA = std::make_shared<A>();

  VERIFY( spA.get() != 0 );
}

void test02()
{
  std::shared_ptr<B> spB = std::make_shared<B>(99);

  VERIFY( spB->i == 99 );
}

int main()
{
  test01();
  test02();
  return 0;
}
