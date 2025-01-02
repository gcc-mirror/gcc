// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-debug-mode "" }

// PR libstdc++/89608

#include <unordered_set>

int main()
{
  std::unordered_set<int> myset;
  myset.reserve(2);
  myset.insert(0);
  myset.insert(1);

  int i = 2;
  for (auto it = myset.begin(), end = myset.end(); it != end; ++it)
    myset.insert(i++);

  return 0;
}
