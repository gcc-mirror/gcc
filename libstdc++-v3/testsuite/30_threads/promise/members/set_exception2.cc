// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

void test01()
{
  bool test = false;

  std::promise<int> p1;
  std::future<int> f1 = p1.get_future();

  p1.set_exception(std::copy_exception(0));

  try
  {
    p1.set_exception(std::copy_exception(1));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  try
  {
    f1.get();
    test = false;
  }
  catch(int i)
  {
    VERIFY( i == 0 );
  }

  VERIFY( test );
}

void test02()
{
  bool test = false;

  std::promise<int> p1;
  std::future<int> f1 = p1.get_future();

  p1.set_value(2);

  try
  {
    p1.set_exception(std::copy_exception(0));
    VERIFY( false );
  }
  catch (std::future_error& e)
  {
    VERIFY(e.code() ==
        std::make_error_code(std::future_errc::promise_already_satisfied));
    test = true;
  }

  VERIFY( test );
}

int main()
{
  test01();
  test02();
  return 0;
}
