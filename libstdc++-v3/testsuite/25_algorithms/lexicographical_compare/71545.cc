// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do link { target c++11 } }
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PROFILE" } }

#include <algorithm>

struct X { };

bool operator<(X, int) { return true; }
bool operator<(int, X) { return false; }

bool operator<(X, X); // undefined (PR libstdc++/71545)

int main()
{
  X x[1];
  int i[1];
  std::lexicographical_compare(x, x+1, i, i+1);
}
