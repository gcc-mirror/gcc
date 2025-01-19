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

// { dg-do run { target c++20 } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

#include <latch>
#include <atomic>
#include <thread>
#include <testsuite_hooks.h>

void
test01()
{
  std::latch l(3);

  VERIFY( !l.try_wait() );

  auto fn = [&]
  {
    l.count_down();
  };

  std::thread t0(fn);
  std::thread t1(fn);

  l.arrive_and_wait();
  t0.join();
  t1.join();

  VERIFY( l.try_wait() );
}

void
test02()
{
  std::latch l(3);
  std::thread t([&]
    {
      l.count_down();
    });

  l.arrive_and_wait(2);
  t.join();
  VERIFY( l.try_wait() );
}

int main()
{
  test01();
  test02();
  return 0;
}
