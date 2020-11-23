// { dg-options "-std=gnu++2a -pthread" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

// Copyright (C) 2020 Free Software Foundation, Inc.
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
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <type_traits>

#include <testsuite_hooks.h>

template<typename Tp>
Tp check_wait_notify(Tp val1, Tp val2)
{
  using namespace std::literals::chrono_literals;

  std::mutex m;
  std::condition_variable cv;

  Tp aa = val1;
  std::atomic_ref<Tp> a(aa);
  std::thread t([&]
		{
		  cv.notify_one();
		  a.wait(val1);
		  if (a.load() != val2)
		    a = val1;
		});
  std::unique_lock<std::mutex> l(m);
  cv.wait(l);
  std::this_thread::sleep_for(100ms);
  a.store(val2);
  a.notify_one();
  t.join();
  return a.load();
}

template<typename Tp,
	 bool = std::is_integral_v<Tp>
	 || std::is_floating_point_v<Tp>>
struct check;

template<typename Tp>
struct check<Tp, true>
{
  check()
  {
    Tp a = 0;
    Tp b = 42;
    VERIFY(check_wait_notify(a, b) == b);
  }
};

template<typename Tp>
struct check<Tp, false>
{
  check(Tp b)
  {
    Tp a;
    VERIFY(check_wait_notify(a, b) == b);
  }
};

int
main ()
{
  check<long>();
  check<double>();
  return 0;
}
