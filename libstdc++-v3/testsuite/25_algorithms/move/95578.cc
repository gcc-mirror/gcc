// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;

template<typename It>
struct sentinel
{
  It it;

  friend bool
  operator==(It x, sentinel y)
  { return x == y.it; }
};

void
test01()
{
  std::vector<int> v = {1,2,3,4,5}, w = {0,0,0,0,0};
  ranges::subrange sr = {v.begin(), sentinel{v.end()}};
  ranges::move(sr, w.begin());
  VERIFY( ranges::equal(w, (int[]){1,2,3,4,5}) );
}

void
test02()
{
  using std::reverse_iterator;
  std::vector<int> v = {1,2,3,4,5}, w = {0,0,0,0,0};
  ranges::subrange sr
    = {reverse_iterator{v.end()}, sentinel{reverse_iterator{v.begin()}}};
  ranges::move(sr, reverse_iterator{w.end()});
  VERIFY( ranges::equal(w, (int[]){1,2,3,4,5}) );
}

int
main()
{
  test01();
  test02();
}
