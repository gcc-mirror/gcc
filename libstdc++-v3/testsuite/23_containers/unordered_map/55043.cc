// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

// libstdc++/55043

#include <unordered_map>
#include <vector>

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&) = default;
};

using hash = std::hash<int>;
using equal = std::equal_to<int>;

template<typename Alloc>
  using test_type = std::unordered_map<int, MoveOnly, hash, equal, Alloc>;

void test01()
{
  typedef test_type<std::allocator<MoveOnly>> uim;
  std::vector<uim> v;
  v.emplace_back(uim());
}

