// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

struct sum {
  typedef int result_type;
  int operator()(int i, int& j, const int& k) { return i + j + k; }
};

void test01()
{
  bool test __attribute__((unused)) = true;

  using namespace std;

  int a = 1;
  int b = 10;
  int c = 100;
  future<int> f1 = async(launch::async|launch::deferred, sum(), a, ref(b), cref(c));
  future<int> f2 = async(sum(), a, ref(b), cref(c));

  VERIFY( f1.valid() );
  VERIFY( f2.valid() );
  int r1 = f1.get();
  int r2 = f2.get();
  VERIFY( r1 == r2 );
}

int main()
{
  test01();
  return 0;
}
