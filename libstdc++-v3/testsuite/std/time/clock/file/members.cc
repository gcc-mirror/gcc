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

// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

void
test01()
{
  auto sys_now = std::chrono::system_clock::now();
  auto file_now = std::chrono::file_clock::now();
  auto d1 = std::chrono::file_clock::to_sys(file_now) - sys_now;
  VERIFY( d1 < std::chrono::seconds(1) );
  auto d2 = file_now - std::chrono::file_clock::from_sys(sys_now);
  VERIFY( d2 == d1 );
}

void
test02()
{
  using namespace std::chrono;

  file_time<file_clock::duration> t = file_clock::now();
  file_time<seconds> s = floor<seconds>(t);
  VERIFY( t - s < 1s );
}

void
test03()
{
  using namespace std::chrono;
  auto st = sys_days(2024y/January/21);
  VERIFY( file_clock::to_sys(file_clock::from_sys(st)) == st );
}

int
main()
{
  test01();
  test02();
  test03();
}
