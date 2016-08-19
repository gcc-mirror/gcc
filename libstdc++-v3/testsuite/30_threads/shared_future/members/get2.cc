// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2009-2016 Free Software Foundation, Inc.
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
#include <exception>
#include <testsuite_hooks.h>

int value = 99;

void test01()
{
  bool test __attribute__((unused)) = true;

  std::promise<int> p1;
  std::shared_future<int> f1(p1.get_future());
  std::shared_future<int> f2(f1);

  p1.set_exception(std::make_exception_ptr(value));
  try
  {
    (void) f1.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
  try
  {
    (void) f2.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
}

void test02()
{
  bool test __attribute__((unused)) = true;

  std::promise<int&> p1;
  std::shared_future<int&> f1(p1.get_future());
  std::shared_future<int&> f2(f1);

  p1.set_exception(std::make_exception_ptr(value));
  try
  {
    (void) f1.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
  try
  {
    (void) f2.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
}

void test03()
{
  bool test __attribute__((unused)) = true;

  std::promise<void> p1;
  std::shared_future<void> f1(p1.get_future());
  std::shared_future<void> f2(f1);

  p1.set_exception(std::make_exception_ptr(value));
  try
  {
    f1.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
  try
  {
    f2.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
}

int main()
{
  test01();
  test02();
  test03();

  return 0;
}
