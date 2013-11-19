// { dg-options "-std=gnu++0x" }

// 2010-05-20  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_api.h>

void f1(int) { }

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace __gnu_test;
  
  OverloadedAddress* ao1 = new OverloadedAddress();
  OverloadedAddress& o1 = *ao1;

  VERIFY( std::addressof(o1) == ao1 );

  const OverloadedAddress* ao2 = new OverloadedAddress();
  const OverloadedAddress& o2 = *ao2;

  VERIFY( std::addressof(o2) == ao2 );

  VERIFY( std::addressof(f1) == &f1 );
}

int main()
{
  test01();
  return 0;
}
