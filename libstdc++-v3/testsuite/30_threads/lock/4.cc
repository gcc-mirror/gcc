// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

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


#include <mutex>
#include <testsuite_hooks.h>

struct unreliable_lock
{
  std::mutex m;
  std::unique_lock<std::mutex> l;

  static int count;
  static int throw_on;
  static int lock_on;

  unreliable_lock() : l(m, std::defer_lock) { }

  ~unreliable_lock()
  {
    bool test __attribute__((unused)) = true;
    VERIFY( !l.owns_lock() );
  }

  void lock()
  {
    if (count == throw_on)
      throw throw_on;
    ++count;
    l.lock();
  }
  bool try_lock()
  {
    if (count == throw_on)
      throw throw_on;
    std::unique_lock<std::mutex> l2(m, std::defer_lock);
    if (count == lock_on)
      l2.lock();
    ++count;
    return l.try_lock();
  }

  void unlock()
  {
    bool test __attribute__((unused)) = true;
    VERIFY( l.owns_lock() );
    l.unlock();
  }

};

int unreliable_lock::count = 0;
int unreliable_lock::throw_on = -1;
int unreliable_lock::lock_on = -1;

void test01()
{
  bool test __attribute__((unused)) = true;

  unreliable_lock l1, l2, l3;

  try
    {
      unreliable_lock::count = 0;
      std::lock(l1, l2, l3);
      VERIFY( unreliable_lock::count == 3 );
      l1.unlock();
      l2.unlock();
      l3.unlock();
    }
  catch (...)
    {
      VERIFY( false );
    }
}

void test02()
{
  bool test __attribute__((unused)) = true;

  // test behaviour when a lock is already held
  try
    {
      unreliable_lock::lock_on = 1;
      while (unreliable_lock::lock_on < 3)
      {
        unreliable_lock::count = 0;
        unreliable_lock l1, l2, l3;
        std::lock(l1, l2, l3);
        VERIFY( unreliable_lock::count > 3 );
        l1.unlock();
        l2.unlock();
        l3.unlock();
        ++unreliable_lock::lock_on;
      }
    }
  catch (...)
    {
      VERIFY( false );
    }
}

void test03()
{
  // test behaviour when an exception is thrown
  unreliable_lock::throw_on = 0;
  while (unreliable_lock::throw_on < 3)
  {
    unreliable_lock::count = 0;
    unreliable_lock l1, l2, l3;
    bool test = false;
    try
      {
        std::lock(l1, l2, l3);
      }
    catch (...)
      {
        test = true;
      }
    VERIFY( test );
    ++unreliable_lock::throw_on;
  }
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
