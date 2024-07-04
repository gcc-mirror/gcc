// { dg-options "-g -O0 -Wno-unused" }
// { dg-do run { target c++17 } }

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include <list>
#include <set>
#include <map>
#include <string>
#include <any>
#include <iostream>

int main()
{
  std::list<std::string> list{"a"};
  std::list<std::string>::iterator lit = list.begin();
  // { dg-final { note-test lit {"a"} } }

  std::forward_list<std::string> flist{"b"};
  std::forward_list<std::string>::iterator flit = flist.begin();
  // { dg-final { note-test flit {"b"} } }

  std::map<int, int> m{ {1, 2} };
  auto mit = m.begin();
  // { dg-final { note-test mit {{first = 1, second = 2}} } }

  std::any a = m;
  // { dg-final { regexp-test a {std::any containing std::(__debug::)?map with 1 element = {\[1\] = 2}} } }

  std::set<int> s{1, 2};
  auto sit = s.begin();
  // { dg-final { note-test sit {1} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}
// { dg-final { gdb-test SPOT } }
