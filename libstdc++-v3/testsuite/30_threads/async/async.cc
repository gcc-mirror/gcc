// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2017 Free Software Foundation, Inc.
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

using namespace std;

void work(mutex& m)
{
  unique_lock<mutex> l(m);
}

void test01()
{
  mutex m;
  unique_lock<mutex> l(m);
  future<void> f1 = async(launch::async, &work, ref(m));
  l.unlock();  // allow async thread to proceed
  f1.get();    // wait for it to finish
}

void test02()
{
  mutex m;
  unique_lock<mutex> l(m);
  future<void> f1 = async(launch::async, &work, ref(m));
  std::future_status status;
  status = f1.wait_for(std::chrono::milliseconds(1));
  VERIFY( status == std::future_status::timeout );
  status = f1.wait_until(std::chrono::system_clock::now());
  VERIFY( status == std::future_status::timeout );
  l.unlock();  // allow async thread to proceed
  f1.wait();   // wait for it to finish
  status = f1.wait_for(std::chrono::milliseconds(0));
  VERIFY( status == std::future_status::ready );
  status = f1.wait_until(std::chrono::system_clock::now());
  VERIFY( status == std::future_status::ready );
}

int main()
{
  test01();
  test02();
  return 0;
}
