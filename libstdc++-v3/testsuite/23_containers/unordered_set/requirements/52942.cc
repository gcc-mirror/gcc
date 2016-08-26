// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <functional>

struct TFoo {};

struct TFoo_hash
{
  std::size_t operator()(const TFoo &) const { return 0; }
};

void f1(std::unordered_set<TFoo, TFoo_hash> &) {}

void f2()
{
  std::unordered_set<TFoo, TFoo_hash> set1;
  std::bind(f1, std::ref(set1));
}
