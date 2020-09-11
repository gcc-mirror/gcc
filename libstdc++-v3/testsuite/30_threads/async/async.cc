// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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
  status = f1.wait_until(std::chrono::steady_clock::now());
  VERIFY( status == std::future_status::timeout );
  l.unlock();  // allow async thread to proceed
  f1.wait();   // wait for it to finish
  status = f1.wait_for(std::chrono::milliseconds(0));
  VERIFY( status == std::future_status::ready );
  status = f1.wait_until(std::chrono::system_clock::now());
  VERIFY( status == std::future_status::ready );
  status = f1.wait_until(std::chrono::steady_clock::now());
  VERIFY( status == std::future_status::ready );
}

// This clock behaves exactly the same as steady_clock, but it is not
// steady_clock which means that the generic clock overload of
// future::wait_until is used.
struct steady_clock_copy
{
  using rep = std::chrono::steady_clock::rep;
  using period = std::chrono::steady_clock::period;
  using duration = std::chrono::steady_clock::duration;
  using time_point = std::chrono::time_point<steady_clock_copy, duration>;
  static constexpr bool is_steady = true;

  static time_point now()
  {
    const auto steady = std::chrono::steady_clock::now();
    return time_point{steady.time_since_epoch()};
  }
};

// This test is prone to failures if run on a loaded machine where the
// kernel decides not to schedule us for several seconds. It also
// assumes that no-one will warp CLOCK whilst the test is
// running when CLOCK is std::chrono::system_clock.
template<typename CLOCK>
void test03()
{
  auto const start = CLOCK::now();
  future<void> f1 = async(launch::async, []() {
      std::this_thread::sleep_for(std::chrono::seconds(2));
    });
  std::future_status status;

  status = f1.wait_for(std::chrono::milliseconds(500));
  VERIFY( status == std::future_status::timeout );

  status = f1.wait_until(start + std::chrono::seconds(1));
  VERIFY( status == std::future_status::timeout );

  status = f1.wait_until(start + std::chrono::seconds(5));
  VERIFY( status == std::future_status::ready );

  auto const elapsed = CLOCK::now() - start;
  VERIFY( elapsed >= std::chrono::seconds(2) );
  VERIFY( elapsed < std::chrono::seconds(5) );
}

// This clock is supposed to run at a tenth of normal speed, but we
// don't have to worry about rounding errors causing us to wake up
// slightly too early below if we actually run it at an eleventh of
// normal speed. It is used to exercise the
// __atomic_futex_unsigned::_M_load_when_equal_until overload that
// takes an arbitrary clock.
struct slow_clock
{
  using rep = std::chrono::steady_clock::rep;
  using period = std::chrono::steady_clock::period;
  using duration = std::chrono::steady_clock::duration;
  using time_point = std::chrono::time_point<slow_clock, duration>;
  static constexpr bool is_steady = true;

  static time_point now()
  {
    const auto steady = std::chrono::steady_clock::now();
    return time_point{steady.time_since_epoch() / 11};
  }
};

void test04()
{
  using namespace std::chrono;

  auto const slow_start = slow_clock::now();
  future<void> f1 = async(launch::async, []() {
      std::this_thread::sleep_for(std::chrono::seconds(2));
    });

  // Wait for ~1s
  {
    auto const steady_begin = steady_clock::now();
    auto const status = f1.wait_until(slow_start + milliseconds(100));
    VERIFY(status == std::future_status::timeout);
    auto const elapsed = steady_clock::now() - steady_begin;
    VERIFY(elapsed >= seconds(1));
    VERIFY(elapsed < seconds(2));
  }

  // Wait for up to ~2s more
  {
    auto const steady_begin = steady_clock::now();
    auto const status = f1.wait_until(slow_start + milliseconds(300));
    VERIFY(status == std::future_status::ready);
    auto const elapsed = steady_clock::now() - steady_begin;
    VERIFY(elapsed < seconds(2));
  }
}

int main()
{
  test01();
  test02();
  test03<std::chrono::system_clock>();
  test03<std::chrono::steady_clock>();
  test03<steady_clock_copy>();
  test04();
  return 0;
}
