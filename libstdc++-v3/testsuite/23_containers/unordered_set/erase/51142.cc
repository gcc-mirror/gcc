// Copyright (C) 2011 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

// { dg-do compile }
// { dg-options "-std=gnu++0x" }

#include <unordered_set>

struct X
{
  template<typename T>
  X(T&) {}
};

bool operator==(const X&, const X&) { return false; }

// LWG 2059.
void erasor(std::unordered_set<X>& s, X x)
{
  std::unordered_set<X>::iterator it = s.find(x);
  if (it != s.end())
    s.erase(it);
}
