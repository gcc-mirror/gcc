// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }

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
#include <chrono>
#include <condition_variable>
#include <concepts>
#include <mutex>
#include <thread>

#include <testsuite_hooks.h>

int
main()
{
  using namespace std::literals::chrono_literals;

  std::mutex m;
  std::condition_variable cv;
  std::unique_lock<std::mutex> l(m);

  std::atomic_flag a;
  std::atomic_flag b;
  std::thread t([&]
		{
		  {
		    // This ensures we block until cv.wait(l) starts.
		    std::lock_guard<std::mutex> ll(m);
		  }
		  cv.notify_one();
		  a.wait(false);
		  b.test_and_set();
		  b.notify_one();
		});

  cv.wait(l);
  std::this_thread::sleep_for(100ms);
  a.test_and_set();
  a.notify_one();
  b.wait(false);
  t.join();

  VERIFY( a.test() );
  VERIFY( b.test() );
  return 0;
}
