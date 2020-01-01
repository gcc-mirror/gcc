// { dg-do compile { target c++11 } }

// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


// This file tests explicit instantiation of library containers.

#include <queue>

using std::priority_queue;
using std::vector;

template<typename A>
constexpr bool default_constructible()
{ return std::is_default_constructible<A>::value; }

static_assert(default_constructible<priority_queue<int>>(),
	      "priority_queue<int>");

struct NonDefaultConstructible : vector<int> {
  NonDefaultConstructible(int) { }
};
struct Cmp : std::less<int> {
  Cmp(int) { }
};
static_assert(
    !default_constructible<priority_queue<int, NonDefaultConstructible>>(),
    "priority_queue<int, NonDefaultConstructible>");
static_assert(
    !default_constructible<priority_queue<int, NonDefaultConstructible, Cmp>>(),
    "priority_queue<int, NonDefaultConstructible, Cmp>");
static_assert(
    !default_constructible<priority_queue<int, vector<int>, Cmp>>(),
    "priority_queue<int, vector<int>, Cmp>");
