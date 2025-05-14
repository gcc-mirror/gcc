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

#include <forward_list>

#ifndef __cpp_lib_erase_if
# error "Feature-test macro for erase_if missing in <forward_list>"
#elif __cpp_lib_erase_if < 202002
# error "Feature-test macro for erase_if has wrong value in <forward_list>"
#endif

#include <testsuite_hooks.h>

void
test01()
{
  auto is_odd = [](const int i) { return i % 2 != 0; };

  std::forward_list<int> fl{ 10, 11, 12, 14, 15, 17, 18, 19 };
  auto num = std::erase_if(fl, is_odd);
  std::forward_list<int> t{ 10, 12, 14, 18 };
  VERIFY( fl == t );
  VERIFY( num == 4 );
}

void
test02()
{
  std::forward_list<int> fl{ 10, 11, 12, 14, 15, 17, 18, 19 };
  auto num = std::erase(fl, 14);
  std::forward_list<int> t{ 10, 11, 12, 15, 17, 18, 19 };
  VERIFY( fl == t );
  VERIFY( num == 1 );
  num = std::erase(fl, 20);
  VERIFY( fl == t );
  VERIFY( num == 0 );
}

// LWG 4135.
// The helper lambda of std::erase for list should specify return type as bool
void
test_lwg4135()
{
  struct Bool {
    Bool() = default;
    Bool(const Bool&) = delete;
    operator bool() const { return false; }
  };

  static Bool b;

  struct Int {
    Bool& operator==(Int) const { return b; }
    void operator==(Int) = delete;
  };

  std::forward_list<Int> l;
  std::erase(l, Int{});
}

int
main()
{
  test01();
  test02();

  return 0;
}
