// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <semaphore>
#include <chrono>
#include <thread>
#include <atomic>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std::chrono_literals;
  std::counting_semaphore<10> s(2);
  s.acquire();

  auto const dur = 250ms;
  {
    auto const at = std::chrono::system_clock::now() + dur;
    auto const t0 = std::chrono::steady_clock::now();
    VERIFY( s.try_acquire_until(at) );
    auto const diff = std::chrono::steady_clock::now() - t0;
    VERIFY( diff < dur );
  }

  {
    auto const at = std::chrono::system_clock::now() + dur;
    auto const t0 = std::chrono::steady_clock::now();
    VERIFY( !s.try_acquire_until(at) );
    auto const diff = std::chrono::steady_clock::now() - t0;
    VERIFY( diff >= dur );
  }
}

void test02()
{
  using namespace std::chrono_literals;
  std::binary_semaphore s(1);
  std::atomic<int> a(0), b(0);
  std::thread t([&] {
    a.wait(0);
    auto const dur = 250ms;
    {
      auto const at = std::chrono::system_clock::now() + dur;
      VERIFY( !s.try_acquire_until(at) );

      b++;
      b.notify_one();
    }

    a.wait(1);
    {
      auto const at = std::chrono::system_clock::now() + dur;
      VERIFY( s.try_acquire_until(at) );
    }
    b++;
    b.notify_one();
  });
  t.detach();

  s.acquire();
  a++;
  a.notify_one();
  b.wait(0);
  s.release();
  a++;
  a.notify_one();

  b.wait(1);
}

int main()
{
  test01();
  test02();
}
