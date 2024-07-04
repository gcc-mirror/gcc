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
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

#include <semaphore>
#ifdef _GLIBCXX_HAVE_POSIX_SEMAPHORE
#include <chrono>
#include <thread>
#include <atomic>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std::chrono_literals;
  std::__platform_semaphore s(2);
  s._M_acquire();

  auto const dur = 250ms;
  {
    auto const t0 = std::chrono::steady_clock::now();
    VERIFY( s._M_try_acquire_for(dur) );
    auto const diff = std::chrono::steady_clock::now() - t0;
    VERIFY( diff < dur );
  }

  {
    auto const t0 = std::chrono::steady_clock::now();
    VERIFY( !s._M_try_acquire_for(dur) );
    auto const diff = std::chrono::steady_clock::now() - t0;
    VERIFY( diff >= dur );
  }
}

void test02()
{
  using namespace std::chrono_literals;
  std::__platform_semaphore s(1);
  std::atomic<int> a(0), b(0);
  std::thread t([&] {
    a.wait(0);
    auto const dur = 250ms;
    VERIFY( !s._M_try_acquire_for(dur) );
    b++;
    b.notify_one();

    a.wait(1);
    VERIFY( s._M_try_acquire_for(dur) );
    b++;
    b.notify_one();
  });
  t.detach();

  s._M_acquire();
  a++;
  a.notify_one();
  b.wait(0);
  s._M_release(1);
  a++;
  a.notify_one();

  b.wait(1);
}

void test03()
{
  using namespace std::chrono_literals;
  std::__platform_semaphore s(2);
  s._M_acquire();

  auto const dur = 250ms;
  {
    auto const at = std::chrono::system_clock::now() + dur;
    auto const t0 = std::chrono::steady_clock::now();
    VERIFY( s._M_try_acquire_until(at) );
    auto const diff = std::chrono::steady_clock::now() - t0;
    VERIFY( diff < dur );
  }

  {
    auto const at = std::chrono::system_clock::now() + dur;
    auto const t0 = std::chrono::steady_clock::now();
    VERIFY( !s._M_try_acquire_until(at) );
    auto const diff = std::chrono::steady_clock::now() - t0;
    VERIFY( diff >= dur );
  }
}

void test04()
{
  using namespace std::chrono_literals;
  std::__platform_semaphore s(1);
  std::atomic<int> a(0), b(0);
  std::thread t([&] {
    a.wait(0);
    auto const dur = 250ms;
    {
      auto const at = std::chrono::system_clock::now() + dur;
      VERIFY( !s._M_try_acquire_until(at) );

      b++;
      b.notify_one();
    }

    a.wait(1);
    {
      auto const at = std::chrono::system_clock::now() + dur;
      VERIFY( s._M_try_acquire_until(at) );
    }
    b++;
    b.notify_one();
  });
  t.detach();

  s._M_acquire();
  a++;
  a.notify_one();
  b.wait(0);
  s._M_release(1);
  a++;
  a.notify_one();

  b.wait(1);
}
#endif

int main()
{
#ifdef _GLIBCXX_HAVE_POSIX_SEMAPHORE
  test01();
  test02();
  test03();
  test04();
#endif
  return 0;
}
