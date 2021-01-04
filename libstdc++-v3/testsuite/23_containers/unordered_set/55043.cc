// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <vector>

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&) = default;
};

struct equal
{
  bool
  operator()(const MoveOnly&, const MoveOnly&) const
  { return true; }
};

struct hash
{
  std::size_t
  operator()(const MoveOnly&) const
  { return 0; }
};

template<typename Alloc>
  using test_type = std::unordered_set<MoveOnly, hash, equal, Alloc>;

void test01()
{
  typedef test_type<std::allocator<MoveOnly>> uim;
  std::vector<uim> v;
  v.emplace_back(uim());
}
