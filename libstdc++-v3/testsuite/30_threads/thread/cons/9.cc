// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2017 Free Software Foundation, Inc.
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


#include <thread>
#include <system_error>
#include <testsuite_hooks.h>

int total = 0;

// Functor has internal state.
struct moveable
{
  int i;

  moveable() = default;
  ~moveable() = default;
  moveable(const moveable& c) = delete;
  moveable& operator=(const moveable&) = delete;

  moveable(int j): i(j) { }
  moveable(moveable&& m): i(m.i) { }

  void operator()() const { total += i; }
};

// Two threads called by same functor type, different functor objects
// that have different state. Make sure each thread calls the correct
// functor.
void test09()
{
  try
    {
      // first
      moveable m1(60);
      std::thread t1(std::move(m1));
      t1.join();
      VERIFY( total == 60 );

      // second
      moveable m2(600);
      std::thread t2(std::move(m2));
      t2.join();
      VERIFY( total == 660 ); // Not 120...
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
  test09();
  return 0;
}
