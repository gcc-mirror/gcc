// { dg-do run { target c++20 } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <atomic>
#include <thread>

#include <testsuite_hooks.h>

void
test01()
{
  std::atomic_flag a;
  VERIFY( !a.test() );
  a.wait(true);
  std::thread t([&]
    {
      a.test_and_set();
      a.notify_one();
    });
  a.wait(false);
  t.join();
}

void
test02()
{
  std::atomic_flag a;
  VERIFY( !std::atomic_flag_test(&a) );
  std::atomic_flag_wait(&a, true);
  std::thread t([&]
    {
      std::atomic_flag_test_and_set(&a);
      std::atomic_flag_notify_one(&a);
    });
    std::atomic_flag_wait(&a, false);
    t.join();
}

int
main()
{
  test01();
  test02();
  return 0;
}
