// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

#include <functional>

void* f(std::function<void()>) { return nullptr; }
int f(std::function<void(int)>) { return 1; }

void test01()
{
  void* p __attribute__((unused));
  int i __attribute__((unused));

  p = f([] { });
  i = f([] (int) { });
}

void g(std::function<void()>) { }
void h(std::function<int(int)>) { }

void test02()
{
  g([] { return "ignored"; });
  h([] (char c) { return c; });
}

int main()
{
  test01();
  test02();

  return 0;
}
