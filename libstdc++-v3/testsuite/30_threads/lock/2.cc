// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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
#include <functional>
#include <testsuite_hooks.h>

void locker(std::mutex& m1, std::mutex& m2, std::mutex& m3)
{
  typedef std::unique_lock<std::mutex> lock_type;

  lock_type l1(m1, std::defer_lock);
  lock_type l2(m2, std::defer_lock);
  lock_type l3(m3, std::defer_lock);
  std::lock(l1, l2, l3);
  VERIFY( l1.owns_lock() );
  VERIFY( l2.owns_lock() );
  VERIFY( l3.owns_lock() );
}

void test01()
{
  std::mutex m1, m2, m3;
  std::thread t1(locker, std::ref(m1), std::ref(m2), std::ref(m3));
  std::thread t2(locker, std::ref(m3), std::ref(m2), std::ref(m1));
  t1.join();
  t2.join();
}

int main()
{
  test01();
  return 0;
}
