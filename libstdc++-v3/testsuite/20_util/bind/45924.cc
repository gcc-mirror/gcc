// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

// PR libstdc++/45924

struct f
{
  int operator()(int, int) const { return 0; }
};

void test01()
{
  int i = 0;
  using namespace std::placeholders;
  auto b = std::bind<int>(f(), _1, _2);
  auto const bc(b);
  b(i, i);
  bc(i, i);
}

int main()
{
  test01();
  return 0;
}

