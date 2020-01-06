// { dg-do run { target c++14 } }

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#include <experimental/deque>
#include <testsuite_hooks.h>

void
test01()
{
  auto is_odd = [](const int i) { return i % 2 != 0; };

  std::deque<int> d{ 10, 11, 12, 14, 15, 17, 18, 19 };
  std::experimental::erase_if(d, is_odd);
  std::deque<int> t{ 10, 12, 14, 18 };
  VERIFY( d == t );
}

void
test02()
{
  std::deque<int> d{ 10, 11, 12, 14, 15, 17, 18, 19 };
  std::experimental::erase(d, 14);
  std::deque<int> t{ 10, 11, 12, 15, 17, 18, 19 };
  VERIFY( d == t );
  std::experimental::erase(d, 20);
  VERIFY( d == t );
}

int
main()
{
  test01();
  test02();

  return 0;
}
