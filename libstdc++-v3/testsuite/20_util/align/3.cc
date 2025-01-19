// { dg-do run { target c++11 } }

// 2020-09-20 Glen Joseph Fernandes <glenjofe@gmail.com>

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 [ptr.align] (20.6.5): std::align

#include <memory>
#include <testsuite_hooks.h>

void test01()
{
  void* p1 = reinterpret_cast<void*>(5);
  void* p2 = p1;
  std::size_t s1 = 3072;
  std::size_t s2 = s1;
  VERIFY(std::align(1024, static_cast<std::size_t>(-1), p1, s1) == nullptr);
  VERIFY(p1 == p2);
  VERIFY(s1 == s2);
}

void test02()
{
  void* p1 = reinterpret_cast<void*>(1);
  void* p2 = p1;
  std::size_t s1 = -1;
  std::size_t s2 = s1;
  VERIFY(std::align(2, static_cast<std::size_t>(-1), p1, s1) == nullptr);
  VERIFY(p1 == p2);
  VERIFY(s1 == s2);
}

int main()
{
  test01();
  test02();
}
