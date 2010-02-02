// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2009 Free Software Foundation, Inc.
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
  std::future<int> f1(p1.get_future());

  p1.set_exception(std::copy_exception(value));
  try
  {
    (void) f1.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
  VERIFY( !f1.valid() );
}

void test02()
{
  bool test __attribute__((unused)) = true;

  std::promise<int&> p1;
  std::future<int&> f1(p1.get_future());

  p1.set_exception(std::copy_exception(value));
  try
  {
    (void) f1.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
  VERIFY( !f1.valid() );
}

void test03()
{
  bool test __attribute__((unused)) = true;

  std::promise<void> p1;
  std::future<void> f1(p1.get_future());

  p1.set_exception(std::copy_exception(value));
  try
  {
    f1.get();
    VERIFY( false );
  }
  catch (int& e)
  {
    VERIFY( e == value );
  }
  VERIFY( !f1.valid() );
}

int main()
{
  test01();
  test02();
  test03();

  return 0;
}
