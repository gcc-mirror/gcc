// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <unordered_set>

struct audrey2hash : std::hash<int>
{
  audrey2hash() { throw "Seed me, Seymour"; } // must not use default ctor

  audrey2hash(int) { }

  audrey2hash&
  operator=(const audrey2hash&) { throw "Don't assign the plants"; }
};

void test01()
{
  typedef std::unordered_set<int, audrey2hash> test_type;
  test_type::local_iterator it __attribute__((unused));
  test_type c{ {1, 2, 3}, 3u, audrey2hash{1} };
  it = c.begin(0);
}

int main()
{
  test01();
}
