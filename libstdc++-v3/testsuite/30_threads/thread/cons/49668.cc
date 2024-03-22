// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#include <thread>
#include <functional>
#include <testsuite_hooks.h>

struct moveable
{
  moveable() = default;
  moveable(moveable&&) = default;
  moveable(const moveable&) = delete;
};

typedef decltype(std::placeholders::_1) placeholder_type;

void f(moveable, placeholder_type, bool& b) { b = true; }

void test01()
{
  bool test = false;
  std::thread t(f, moveable(), std::placeholders::_1, std::ref(test));
  t.join();
  VERIFY( test );
}

int main()
{
  test01();
}
