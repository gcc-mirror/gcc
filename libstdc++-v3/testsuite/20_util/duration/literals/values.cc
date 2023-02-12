// { dg-do run { target c++14 } }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void
test03()
{
  using namespace std::literals::chrono_literals;

  auto jiffy = 23ns;
  VERIFY( jiffy == std::chrono::nanoseconds(23) );
  auto fjiffy = 23.0ns;
  VERIFY( (fjiffy == std::chrono::duration<long double, std::nano>(23.0L)) );
  auto blip = 14us;
  VERIFY( blip == std::chrono::microseconds(14) );
  auto fblip = 14.0us;
  VERIFY( (fblip == std::chrono::duration<long double, std::micro>(14.0L)) );
  auto bit = 77ms;
  VERIFY( bit == std::chrono::milliseconds(77) );
  auto fbit = 77.0ms;
  VERIFY( (fbit == std::chrono::duration<long double, std::milli>(77.0L)) );
  auto warmup = 33s;
  VERIFY( warmup == std::chrono::seconds(33) );
  auto fwarmup = 33.0s;
  VERIFY( (fwarmup == std::chrono::duration<long double, std::ratio<1,1>>(33.0L)) );
  auto classtime = 50min;
  VERIFY( classtime == std::chrono::minutes(50) );
  auto fclasstime = 50.0min;
  VERIFY( (fclasstime == std::chrono::duration<long double, std::ratio<60,1>>(50.0L)) );
  auto longtime = 1h + 30min;
  VERIFY( longtime == std::chrono::minutes(90) );
  auto flongtime = 1.0h + 30.0min;
  VERIFY( (flongtime == std::chrono::duration<long double, std::ratio<3600,1>>(1.0L)
		      + std::chrono::duration<long double, std::ratio<60,1>>(30.0L)) );
  VERIFY( (flongtime == std::chrono::duration<long double, std::ratio<60,1>>(90.0L)) );
  auto workday = 8h;
  VERIFY( workday == std::chrono::hours(8) );
  auto fworkday = 8.0h;
  VERIFY( (fworkday == std::chrono::duration<long double, std::ratio<3600,1>>(8.0L)) );
  auto immediate = 0s;
  VERIFY( immediate == std::chrono::seconds(0) );
  auto minute_ago = -1min;
  VERIFY( minute_ago == std::chrono::minutes(-1) );
  auto separated = 1'000'000s;
  VERIFY( separated == std::chrono::seconds(1'000'000) );
}

int
main()
{
  test03();
}
