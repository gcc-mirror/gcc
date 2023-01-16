// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2023 Free Software Foundation, Inc.
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
#include <chrono>
#include <testsuite_hooks.h>

std::chrono::system_clock::time_point make_time(int i)
{
  return std::chrono::system_clock::now() + std::chrono::milliseconds(i);
}

void test01()
{
  std::promise<int> p1;
  std::future<int> f1(p1.get_future());

  auto when = make_time(10);
  VERIFY( f1.wait_until(when) == std::future_status::timeout );
  VERIFY( std::chrono::system_clock::now() >= when );

  p1.set_value(1);

  when = make_time(100);
  VERIFY( f1.wait_until(when) == std::future_status::ready );
  VERIFY( std::chrono::system_clock::now() < when );
}

int main()
{
  test01();
  return 0;
}
