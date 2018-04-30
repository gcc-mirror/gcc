// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <condition_variable>
#include <thread>
#include <mutex>
#include <testsuite_hooks.h>

std::mutex mx;
std::condition_variable cv;
int counter = 0;

struct Inc
{
  Inc() { ++counter; }
  ~Inc() { ++counter; }
};


void func()
{
  std::unique_lock<std::mutex> lock{mx};
  std::notify_all_at_thread_exit(cv, std::move(lock));
#if CORRECT_THREAD_LOCAL_DTORS
  // Correct order of thread_local destruction needs __cxa_thread_atexit_impl
  // or similar support from libc.
  static thread_local
#endif
  Inc inc;
}

int main()
{
  std::unique_lock<std::mutex> lock{mx};
  std::thread t{func};
  cv.wait(lock, [&]{ return counter == 2; });
  t.join();
}
