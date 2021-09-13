// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <deque>
#include <testsuite_hooks.h>

#ifndef __cpp_lib_erase_if
# error "Feature-test macro for erase_if missing in <deque>"
#elif __cpp_lib_erase_if < 202002
# error "Feature-test macro for erase_if has wrong value in <deque>"
#endif

void
test01()
{
  auto is_odd = [](const int i) { return i % 2 != 0; };

  std::deque<int> d{ 10, 11, 12, 14, 15, 17, 18, 19 };
  auto num = std::erase_if(d, is_odd);
  std::deque<int> t{ 10, 12, 14, 18 };
  VERIFY( d == t );
  VERIFY( num == 4 );
}

void
test02()
{
  std::deque<int> d{ 10, 11, 12, 14, 15, 17, 18, 19 };
  auto num = std::erase(d, 14);
  std::deque<int> t{ 10, 11, 12, 15, 17, 18, 19 };
  VERIFY( d == t );
  VERIFY( num == 1 );
  num = std::erase(d, 20);
  VERIFY( d == t );
  VERIFY( num == 0 );
}

int
main()
{
  test01();
  test02();

  return 0;
}
