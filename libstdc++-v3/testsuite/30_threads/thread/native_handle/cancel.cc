// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2012-2022 Free Software Foundation, Inc.
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

#include <pthread.h>
#include <thread>
#include <atomic>
#include <functional>

void f(std::atomic<bool>& started)
{
  started = true;
  while (true)
    {
      std::this_thread::sleep_for(std::chrono::milliseconds(100));
      // In case the target system doesn't make sleep a cancellation point...
      pthread_testcancel();
    }
}

int main()
{
  std::atomic<bool> started{ false };
  std::thread t(f, std::ref(started));
  while (!started)
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
  pthread_cancel(t.native_handle());
  t.join();
}
