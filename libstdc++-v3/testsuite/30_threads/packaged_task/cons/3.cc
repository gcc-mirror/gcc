// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-gnu* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

// LWG 2067. packaged_task should have deleted copy c'tor with const parameter

#include <future>
#include <thread>
#include <testsuite_hooks.h>

template<typename F>
std::future<typename std::result_of<F()>::type> spawn_task(F f)
{
  typedef typename std::result_of<F()>::type result_type;
  std::packaged_task<result_type()> task(std::move(f));
  std::future<result_type> res(task.get_future());
  std::thread(std::move(task)).detach();
  return res;
}

int get_res()
{
  return 42;
}

void test01()
{
  auto f = spawn_task(get_res);
  VERIFY( f.get() == get_res() );
}

int main()
{
  test01();
}
