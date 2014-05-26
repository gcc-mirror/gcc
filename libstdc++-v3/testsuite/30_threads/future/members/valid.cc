// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;

  std::future<int> f0;
  VERIFY( !f0.valid() );

  std::promise<int> p1;
  std::future<int> f1(p1.get_future());

  VERIFY( f1.valid() );

  p1.set_value(1);

  VERIFY( f1.valid() );

  f1 = std::move(f0);

  VERIFY( !f1.valid() );
  VERIFY( !f0.valid() );
}

int main()
{
  test01();
  return 0;
}
