// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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
#include <thread>

using mutex_type = std::timed_mutex;

mutex_type m;

void f()
{
  std::lock_guard<mutex_type> l(m);
}

int main()
{
  std::thread t1(f);
  std::thread t2(f);
  t1.join();
  t2.join();
}
