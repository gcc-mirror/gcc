// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <variant>

struct moveonly {
  moveonly() noexcept { }
  moveonly(moveonly&&) noexcept { }
  moveonly& operator=(moveonly&&) noexcept { return *this; }
  ~moveonly() { }
};

void
test01()
{
  std::variant<moveonly> v1, v2;
  v1 = std::move(v2); // PR libstdc++/85183
}
