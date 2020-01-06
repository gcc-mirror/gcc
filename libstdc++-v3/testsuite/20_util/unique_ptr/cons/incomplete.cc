// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

#include <memory>

struct Incomplete;

void f(void** p)
{
  ::new (p[0]) std::unique_ptr<Incomplete>();
  ::new (p[1]) std::unique_ptr<Incomplete[]>();

  // PR libstdc++/87704
  ::new (p[2]) std::unique_ptr<Incomplete>(nullptr);
  ::new (p[3]) std::unique_ptr<Incomplete[]>(nullptr);
}
