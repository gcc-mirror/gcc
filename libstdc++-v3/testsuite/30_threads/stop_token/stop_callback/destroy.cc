// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a -pthread"  }
// { dg-add-options libatomic }
// { dg-require-effective-target c++2a }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

#include <stop_token>
#include <atomic>
#include <thread>
#include <testsuite_hooks.h>

struct F
{
  static std::atomic<int> stage;

  F(int) { }

  ~F()
  {
    // PR libstdc++/92895
    // Callback function must not be destroyed while still executing.
    VERIFY( stage == 4 );
  }

  void operator()() const noexcept
  {
    stage = 2; // trigger destructor of stop_callback that owns *this
    while (stage == 2)
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    std::this_thread::sleep_for(std::chrono::milliseconds(500));
    stage = 4; // destructor checks for this
  }
};

std::atomic<int> F::stage{0};

void
test01()
{
  std::stop_source ssrc;
  std::stop_token stok = ssrc.get_token();
  std::thread t1([&ssrc] {
    while (F::stage == 0)
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    ssrc.request_stop();
    while (F::stage != 5)
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
  });

  std::thread t2([&ssrc] {
    std::stop_callback<F> cb(ssrc.get_token(), 0);
    F::stage = 1; // trigger stop request in other thread, which runs callback
    while (F::stage == 1)
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    F::stage = 3;
    // stop_callback destructor should block until callback completes
  });

  t2.join();
  F::stage = 5; // allow first thread to exit
  t1.join();
}

int main()
{
  test01();
}
