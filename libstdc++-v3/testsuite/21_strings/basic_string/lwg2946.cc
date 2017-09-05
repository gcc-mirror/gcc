// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2017 Free Software Foundation, Inc.
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

#include <string>

int main()
{
  std::string s({"abc", 1});
  s = {"abc", 1};
  s += {"abc", 1};
  s.append({"abc", 1});
  s.assign({"abc", 1});
  s.insert(0, {"abc", 1});
  s.replace(0, 1, {"abc", 1});
  s.replace(s.cbegin(), s.cbegin(), {"abc", 1});
  s.find({"abc", 1});
  s.rfind({"abc", 1});
  s.find_first_of({"abc", 1});
  s.find_last_of({"abc", 1});
  s.find_first_not_of({"abc", 1});
  s.find_last_not_of({"abc", 1});
  s.compare({"abc", 1});
  s.compare(0, 1, {"abc", 1});
}
