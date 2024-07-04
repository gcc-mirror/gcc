// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

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


#include <future>
#include <testsuite_hooks.h>

void test01()
{
  std::promise<int> p1;
  std::promise<int> p2;
  p1.set_value(1);
  p1.swap(p2);
  auto delay = std::chrono::milliseconds(1);
  VERIFY( p1.get_future().wait_for(delay) == std::future_status::timeout );
  VERIFY( p2.get_future().wait_for(delay) == std::future_status::ready );
}

int main()
{
  test01();
  return 0;
}
