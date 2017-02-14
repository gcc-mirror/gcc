// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

int copies;
int copies_cmp;

struct Obj
{
  Obj() = default;
  Obj(const Obj&) { ++copies; }
};

std::future<Obj> f1;

bool ready(std::future<Obj>& f)
{
  return f.wait_for(std::chrono::milliseconds(1)) == std::future_status::ready;
}

void test01()
{
  std::promise<Obj> p1;
  f1 = p1.get_future();

  p1.set_value_at_thread_exit( {} );

  copies_cmp = copies;

  VERIFY( !ready(f1) );
}

int main()
{
  std::thread t{test01};
  t.join();
  VERIFY( ready(f1) );
  VERIFY( copies == copies_cmp );
}
