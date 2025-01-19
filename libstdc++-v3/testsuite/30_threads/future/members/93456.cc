// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

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


#include <future>
#include <thread>
#include <chrono>
#include <climits>
#include <testsuite_hooks.h>

namespace chrono = std::chrono;

void test01()
{
  std::future<void> fut = std::async(std::launch::async, [] {
    std::this_thread::sleep_for(chrono::seconds(4));
  });

  // A time in the distant future, but which overflows 32-bit time_t:
  auto then = chrono::system_clock::now() + chrono::seconds(UINT_MAX + 2LL);
  auto status = fut.wait_until(then);
  // The wait_until call should have waited for the result to be ready.
  // If converting the time_point to time_t overflows, it will timeout.
  VERIFY(status == std::future_status::ready);
}

int main()
{
  test01();
}
