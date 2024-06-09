// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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
#include <testsuite_tr1.h>

void test01()
{
  using std::packaged_task;
  using namespace __gnu_test;

  packaged_task<int ()> p1;
  VERIFY( !p1.valid() );
  packaged_task<int& ()> p2;
  VERIFY( !p2.valid() );
  packaged_task<void ()> p3;
  VERIFY( !p3.valid() );
  packaged_task<ClassType ()> p4;
  VERIFY( !p4.valid() );
  packaged_task<AbstractClass& (int)> p5;
  VERIFY( !p5.valid() );
}

int main()
{
  test01();
  return 0;
}
