// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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


#include <functional> // std::unary_function, std::ref
#include <thread>
#include <system_error>
#include <testsuite_hooks.h>

struct nonconst : public std::unary_function<std::thread::id&, void>
{
  void operator()(std::thread::id& id)
  {
    id = std::this_thread::get_id();
  }
};

void test01()
{
  bool test __attribute__((unused)) = true;

  try
  {
    std::thread::id t1_id1;
    nonconst c1;
    std::thread t1(std::ref(c1), std::ref(t1_id1));
    std::thread::id t1_id2 = t1.get_id();
    VERIFY( t1.joinable() );
    t1.join();
    VERIFY( !t1.joinable() );
    VERIFY( t1_id1 == t1_id2 );

    std::thread::id t2_id1;
    nonconst c2;
    std::thread t2(c2, std::ref(t2_id1));
    std::thread::id t2_id2 = t2.get_id();
    VERIFY( t2.joinable() );
    t2.join();
    VERIFY( !t2.joinable() );
    VERIFY( t2_id1 == t2_id2 );
  }
  catch (const std::system_error&)
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
  return 0;
}
