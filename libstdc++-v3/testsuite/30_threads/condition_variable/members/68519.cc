// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

#include <condition_variable>
#include <testsuite_hooks.h>

// PR libstdc++/68519

void
test_wait_for()
{
  std::mutex mx;
  std::condition_variable cv;

  for (int i = 0; i < 3; ++i)
  {
    std::unique_lock<std::mutex> l(mx);
    auto start = std::chrono::system_clock::now();
    cv.wait_for(l, std::chrono::duration<float>(1), [] { return false; });
    auto t = std::chrono::system_clock::now();
    VERIFY( (t - start) >= std::chrono::seconds(1) );
  }
}

// In order to ensure that the delta calculated in the arbitrary clock overload
// of condition_variable::wait_until fits accurately in a float, but the result
// of adding it to steady_clock with a float duration does not, this clock
// needs to use a more recent epoch.
struct recent_epoch_float_clock
{
  using duration = std::chrono::duration<float>;
  using rep = duration::rep;
  using period = duration::period;
  using time_point
    = std::chrono::time_point<recent_epoch_float_clock, duration>;
  static constexpr bool is_steady = true;

  static const std::chrono::steady_clock::time_point epoch;

  static time_point now()
  {
    const auto steady = std::chrono::steady_clock::now();
    return time_point{steady - epoch};
  }
};

const std::chrono::steady_clock::time_point recent_epoch_float_clock::epoch =
  std::chrono::steady_clock::now();

void
test_wait_until()
{
  using clock = recent_epoch_float_clock;

  std::mutex mx;
  std::condition_variable cv;

  for (int i = 0; i < 3; ++i)
  {
    std::unique_lock<std::mutex> l(mx);
    const auto start = clock::now();
    const auto wait_time = start + std::chrono::duration<float>{1.0};

    // In theory we could get a spurious wakeup, but in practice we won't.
    const auto result = cv.wait_until(l, wait_time);

    VERIFY( result == std::cv_status::timeout );
    const auto elapsed = clock::now() - start;
    VERIFY( elapsed >= std::chrono::seconds(1) );
  }
}

int
main()
{
  test_wait_for();
  test_wait_until();
}
