// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <unordered_map>

void
test01()
{
  using namespace std;
  unordered_multimap<int, int, equal_to<int>, hash<int>> c2;
  c2.find(2); // { dg-error "here" }
}

// { dg-error "hash function must be invocable" "" { target *-*-* } 0 }
// { dg-error "key equality predicate must be invocable" "" { target *-*-* } 0 }
// { dg-prune-output "use of deleted function" }
// { dg-prune-output "no match for call" }
