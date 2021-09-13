// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// NB: This issue affected only debug-mode.

// { dg-do compile }

// libstdc++/34730

#include <string>
#include <vector>
#include <algorithm>

using namespace std;

typedef pair<int, string> intstring;

struct intstrcmp
{
  bool
  operator()(const string& x, const intstring& y) const
  { return x < y.second; }

  bool
  operator()(const intstring& x, const string& y) const
  { return x.second < y; }
};

void test01()
{
  vector<string> vec1;
  vector<intstring> vec2;
  vector<intstring> vec3;
  set_intersection(vec2.begin(), vec2.end(),
		   vec1.begin(), vec1.end(),
		   back_inserter(vec3), intstrcmp());
}
