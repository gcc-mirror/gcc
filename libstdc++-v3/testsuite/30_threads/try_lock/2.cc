// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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


#include <mutex>
#include <system_error>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  try
    {
      std::mutex m1, m2, m3;
      m1.lock();
      int result = std::try_lock(m1, m2, m3);
      VERIFY( result == 0 );
      m1.lock();
      m2.lock();
      m3.lock();
    }
  catch (const std::system_error& e)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }
}

void test02()
{
  bool test __attribute__((unused)) = true;

  try
    {
      std::mutex m1, m2, m3;
      m2.lock();
      int result = std::try_lock(m1, m2, m3);
      VERIFY( result == 1 );
      m1.lock();
      m2.lock();
      m3.lock();
    }
  catch (const std::system_error& e)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }
}

void test03()
{
  bool test __attribute__((unused)) = true;

  try
    {
      std::mutex m1, m2, m3;
      m3.lock();
      int result = std::try_lock(m1, m2, m3);
      VERIFY( result == 2 );
      m1.lock();
      m2.lock();
      m3.lock();
    }
  catch (const std::system_error& e)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
