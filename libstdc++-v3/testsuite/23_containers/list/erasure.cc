// { dg-do run { target c++20 } }
// { dg-add-options no_pch }

// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

#include <list>

#ifndef __cpp_lib_erase_if
# error "Feature-test macro for erase_if missing in <list>"
#elif __cpp_lib_erase_if < 202002
# error "Feature-test macro for erase_if has wrong value in <list>"
#endif

#include <testsuite_hooks.h>

void
test01()
{
  auto is_odd = [](const int i) { return i % 2 != 0; };

  std::list<int> l{ 10, 11, 12, 14, 15, 17, 18, 19 };
  std::erase_if(l, is_odd);
  std::list<int> t{ 10, 12, 14, 18 };
  VERIFY( l == t );
}

void
test02()
{
  std::list<int> l{ 0, 11, 0, 0, 22, 33, 0, 0, 44, 0 };
  auto num = std::erase(l, 0);
  std::list<int> t{ 11, 22, 33, 44 };
  VERIFY( l == t );
  VERIFY( num == 6 );
  num = std::erase(l, 55);
  VERIFY( l == t );
  VERIFY( num == 0 );
}

int
main()
{
  test01();
  test02();

  return 0;
}
