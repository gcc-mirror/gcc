// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }
// { dg-require-sched-yield "" }
// { dg-require-sleep "" }

// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

// PR libstdc++/53830
// Test for deadlock in condition_variable_any::wait_for

#include <thread>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <atomic>

std::mutex mutex;
std::condition_variable_any cv;

std::atomic<int> barrier(0);

// waits for data from another thread
void wait_for_data()
{
  std::unique_lock<std::mutex> lock(mutex);
  barrier = 1;
  cv.wait_for(lock, std::chrono::milliseconds(100), []{ return false; });
  // read data
}

// passes data to waiting thread
void provide_data()
{
  while (barrier == 0)
    std::this_thread::yield();
  std::unique_lock<std::mutex> lock(mutex);
  // pass data
  std::this_thread::sleep_for(std::chrono::seconds(1));
  cv.notify_one();
}

int main()
{
  std::thread thread1(wait_for_data);
  provide_data();
  thread1.join();
  return 0;
}

