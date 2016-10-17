// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* powerpc-ibm-aix* } }
// { dg-require-effective-target c++11 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-time "" }

#include <thread>
#include <chrono>
#include <atomic>
#include <cstdint>
#include <signal.h>
#include <testsuite_hooks.h>

void
test01()
{
  std::this_thread::sleep_for(std::chrono::seconds(0));
  std::this_thread::sleep_for(std::chrono::seconds(-1));
  std::this_thread::sleep_for(std::chrono::duration<std::uint64_t>::zero());
}

void
test02()
{
  // test interruption of this_thread::sleep_for() by a signal
  struct sigaction sa{ };
  sa.sa_handler = +[](int) { };
  sigaction(SIGUSR1, &sa, 0);
  bool result = false;
  std::atomic<bool> sleeping{false};
  std::thread t([&result, &sleeping] {
    auto start = std::chrono::system_clock::now();
    auto time = std::chrono::seconds(3);
    sleeping = true;
    std::this_thread::sleep_for(time);
    result = std::chrono::system_clock::now() >= (start + time);
  });
  while (!sleeping) { }
  std::this_thread::sleep_for(std::chrono::milliseconds(500));
  pthread_kill(t.native_handle(), SIGUSR1);
  t.join();
  VERIFY( result );
}

struct slow_clock
{
  using rep = std::chrono::system_clock::rep;
  using period = std::chrono::system_clock::period;
  using duration = std::chrono::system_clock::duration;
  using time_point = std::chrono::time_point<slow_clock, duration>;
  static constexpr bool is_steady = false;

  static time_point now()
  {
    auto real = std::chrono::system_clock::now();
    return time_point{real.time_since_epoch() / 2};
  }
};

void
test03()
{
  // test that this_thread::sleep_until() handles clock adjustments
  auto when = slow_clock::now() + std::chrono::seconds(2);
  std::this_thread::sleep_until(when);
  VERIFY( slow_clock::now() >= when );
}

int
main()
{
  test01();
  test02();
  test03();
}
