// { dg-do compile { target c++11 } }

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

class ClockTime
{
  typedef std::chrono::hours hours;
  typedef std::chrono::minutes minutes;
  typedef std::chrono::seconds seconds;

public:
  hours hours_;
  minutes minutes_;
  seconds seconds_;

  template<typename Rep, typename Period>
    explicit
    ClockTime(const std::chrono::duration<Rep, Period>& d)
    : hours_  (std::chrono::duration_cast<hours>  (d)),
      minutes_(std::chrono::duration_cast<minutes>(d % hours(1))),
      seconds_(std::chrono::duration_cast<seconds>(d % minutes(1))) { }
};

// DR 934.
void test01()
{
  std::chrono::duration<int> d;
  ClockTime ct(d);
}
