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
#include <chrono>
#include <condition_variable>
#include <concepts>
#include <mutex>
#include <thread>
#include <cstring>

#include <testsuite_hooks.h>

template<typename Tp>
Tp check_wait_notify(Tp val1, Tp val2)
  requires std::equality_comparable<Tp>
{
  using namespace std::literals::chrono_literals;

  std::mutex m;
  std::condition_variable cv;
  std::unique_lock<std::mutex> l(m);

  std::atomic<Tp> a(val1);
  std::thread t([&]
		{
		  {
		    // This ensures we block until cv.wait(l) starts.
		    std::lock_guard<std::mutex> ll(m);
		  }
		  cv.notify_one();
		  a.wait(val1);
		  if (a.load() != val2)
		    a = val1;
		});
  cv.wait(l);
  std::this_thread::sleep_for(100ms);
  a.store(val2);
  a.notify_one();
  t.join();
  return a.load();
}

template<typename Tp>
Tp check_wait_notify(Tp val1, Tp val2)
{
  using namespace std::literals::chrono_literals;

  std::mutex m;
  std::condition_variable cv;
  std::unique_lock<std::mutex> l(m);

  std::atomic<Tp> a(val1);
  std::thread t([&]
		{
		  {
		    // This ensures we block until cv.wait(l) starts.
		    std::lock_guard<std::mutex> ll(m);
		  }
		  cv.notify_one();
		  a.wait(val1);
		  auto v = a.load();
		  // TODO this needs to zero padding bits when we can do that
		  if (std::memcmp(&v, &val2, sizeof(Tp)) != 0)
		    a = val1;
		});
  cv.wait(l);
  std::this_thread::sleep_for(100ms);
  a.store(val2);
  a.notify_one();
  t.join();
  return a.load();
}

template<typename Tp>
Tp check_atomic_wait_notify(Tp val1, Tp val2)
  requires std::equality_comparable<Tp>
{
  using namespace std::literals::chrono_literals;

  std::mutex m;
  std::condition_variable cv;
  std::unique_lock<std::mutex> l(m);

  std::atomic<Tp> a(val1);
  std::thread t([&]
		{
		  {
		    // This ensures we block until cv.wait(l) starts.
		    std::lock_guard<std::mutex> ll(m);
		  }
		  cv.notify_one();
		  std::atomic_wait(&a, val1);
		  if (a.load() != val2)
		    a = val1;
		});
  cv.wait(l);
  std::this_thread::sleep_for(100ms);
  a.store(val2);
  std::atomic_notify_one(&a);
  t.join();
  return a.load();
}

template<typename Tp>
Tp check_atomic_wait_notify(Tp val1, Tp val2)
{
  using namespace std::literals::chrono_literals;

  std::mutex m;
  std::condition_variable cv;
  std::unique_lock<std::mutex> l(m);

  std::atomic<Tp> a(val1);
  std::thread t([&]
		{
		  {
		    // This ensures we block until cv.wait(l) starts.
		    std::lock_guard<std::mutex> ll(m);
		  }
		  cv.notify_one();
		  std::atomic_wait(&a, val1);
		  auto v = a.load();
		  // TODO this needs to zero padding bits when we can do that
		  if (std::memcmp(&v, &val2, sizeof(Tp)) != 0)
		    a = val1;
		});
  cv.wait(l);
  std::this_thread::sleep_for(100ms);
  a.store(val2);
  std::atomic_notify_one(&a);
  t.join();
  return a.load();
}

template<typename Tp>
struct check
{
  check(Tp a = 0, Tp b = 42)
  {
    if constexpr (std::equality_comparable<Tp>)
    {
      VERIFY( check_wait_notify(a, b) == b);
      VERIFY( check_atomic_wait_notify(a, b) == b);
    }
    else
    {
      {
	// TODO this needs to zero padding bits when we can do that
	auto v = check_wait_notify(a, b);
	VERIFY( std::memcmp(&v, &b, sizeof(Tp)) == 0 );
      }

      {
	// TODO this needs to zero padding bits when we can do that
	auto v = check_atomic_wait_notify(a, b);
	VERIFY( std::memcmp(&v, &b, sizeof(Tp)) == 0);
      }
    }
  }
};
