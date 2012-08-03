// Copyright (C) 2012
// Free Software Foundation, Inc.
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

// 23.4.4 template class map

// This test verifies that the value type of a map need not be default
// copyable.

// { dg-do compile }
// { dg-options "-std=gnu++11" }

#include <map>

struct Mapped {
    Mapped();
    explicit Mapped(const Mapped&);
};

Mapped & foo()
{
  std::map<int, Mapped> m;
  return m[0];
}
