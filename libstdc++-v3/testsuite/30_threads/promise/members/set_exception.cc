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

// Test that promise::set_exception stores an exception.

#include <future>
#include <testsuite_hooks.h>

void test01()
{
  bool test = false;

  std::promise<int> p1;
  std::future<int> f1 = p1.get_future();

  VERIFY( f1.valid() );

  p1.set_exception(std::make_exception_ptr(0));

  try
  {
    f1.get();
  }
  catch (int)
  {
    test = true;
  }
  VERIFY( test );
  VERIFY( !f1.valid() );
}

void test02()
{
  bool test = false;

  std::promise<int&> p1;
  std::future<int&> f1 = p1.get_future();

  VERIFY( f1.valid() );

  p1.set_exception(std::make_exception_ptr(0));

  try
  {
    f1.get();
  }
  catch (int)
  {
    test = true;
  }
  VERIFY( test );
  VERIFY( !f1.valid() );
}

void test03()
{
  bool test = false;

  std::promise<void> p1;
  std::future<void> f1 = p1.get_future();

  VERIFY( f1.valid() );

  p1.set_exception(std::make_exception_ptr(0));

  try
  {
    f1.get();
  }
  catch (int)
  {
    test = true;
  }
  VERIFY( test );
  VERIFY( !f1.valid() );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
