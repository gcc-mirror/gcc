// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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


#include <future>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>

void test01()
{
  std::promise<int> p1;
  std::future<int> f1 = p1.get_future();

  VERIFY( f1.valid() );

  p1.set_value(0);

  int&& i1 = f1.get();

  VERIFY( i1 == 0 );
}

void test02()
{
  using __gnu_test::rvalstruct;

  std::promise<rvalstruct> p1;
  std::future<rvalstruct> f1 = p1.get_future();

  VERIFY( f1.valid() );

  p1.set_value(rvalstruct(1));

  rvalstruct r1(f1.get());

  VERIFY( !f1.valid() );
  VERIFY( r1.val == 1 );
}


void test03()
{
  std::promise<int&> p1;
  std::future<int&> f1 = p1.get_future();

  VERIFY( f1.valid() );

  int i1 = 0;
  p1.set_value(i1);
  int& i2 = f1.get();

  VERIFY( !f1.valid() );
  VERIFY( &i1 == &i2 );
}

void test04()
{
  std::promise<void> p1;
  std::future<void> f1 = p1.get_future();

  VERIFY( f1.valid() );

  p1.set_value();
  f1.get();

  VERIFY( !f1.valid() );
}

int main()
{
  test01();
  test02();
  test03();
  test04();

  return 0;
}
