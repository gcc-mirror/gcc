// { dg-do compile { target c++17 } }

// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

struct CustomString
{
  std::string data = "foo";
  std::string_view data_view = data;
  operator std::string_view() const {return data_view;}
};

int main()
{
    std::string x;
    CustomString cs;
    x.append("foo", 0, 3);
    x.append(cs, 0, 3);
    x.assign("foo", 0, 3);
    x.assign(cs, 0, 3);
    x.insert(0, "foo", 0, 3);
    x.insert(0, cs, 0, 3);
    x = "bar";
    x.replace(0, 3, "foo", 0, 3);
    x.replace(0, 3, cs, 0, 3);
    x = "bar";
    (void) x.compare(0, 3, "foo", 0, 3);
    (void) x.compare(0, 3, cs, 0, 3);
}
