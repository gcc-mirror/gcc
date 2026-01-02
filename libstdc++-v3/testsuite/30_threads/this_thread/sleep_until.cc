// { dg-do run { target c++11 } }
// { dg-require-sleep "" }

// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

#include <chrono>
#include <thread>
#include <testsuite_hooks.h>

// This tests this_thread::sleep_until without using -pthread

namespace chr = std::chrono;

template <typename Clock>
void
test01()
{
  typename Clock::time_point begin = Clock::now();
  chr::microseconds ms(500);

  std::this_thread::sleep_until(Clock::now() + ms);

  VERIFY( (Clock::now() - begin) >= ms );
}

template <typename Clock>
void
test_negative()
{
  typename Clock::time_point begin = Clock::now();

  typename Clock::time_point tp(-chr::hours(8));
  std::this_thread::sleep_until(tp);

  // That should have completed immediately, but be generous because we don't
  // want spurious failures on busy machines.
  VERIFY( (Clock::now() - begin) < chr::seconds(10) );
}

int main()
{
  test01<chr::steady_clock>();
  test01<chr::system_clock>();
  test_negative<chr::steady_clock>();
  test_negative<chr::system_clock>();
}
