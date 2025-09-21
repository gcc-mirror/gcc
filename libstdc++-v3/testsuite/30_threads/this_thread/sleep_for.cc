// { dg-do run { target c++11 } }
// { dg-require-sleep "" }

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

#include <chrono>
#include <thread>
#include <testsuite_hooks.h>

// This tests this_thread::sleep_until without using -pthread

namespace chr = std::chrono;

void
test01()
{
  chr::system_clock::time_point begin = chr::system_clock::now();
  chr::microseconds ms(500);

  std::this_thread::sleep_for(ms);

  VERIFY( (chr::system_clock::now() - begin) >= ms );
}

void
test_negative()
{
  chr::system_clock::time_point begin = chr::system_clock::now();

  std::this_thread::sleep_for(-chr::hours(8));

  // That should have completed immediately, but be generous because we don't
  // want spurious failures on busy machines.
  VERIFY( (chr::system_clock::now() - begin) < chr::seconds(10) );
}

int main()
{
  test01();
  test_negative();
}
