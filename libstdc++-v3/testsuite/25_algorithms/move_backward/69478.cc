// { dg-do compile { target c++11 } }

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

// PR libstdc++/69478

#include <algorithm>
#include <iterator>

void
test01()
{
  // A move-only type that is also a trivial class.
  struct trivial_rvalstruct
  {
    trivial_rvalstruct() = default;
    trivial_rvalstruct(trivial_rvalstruct&&) = default;
    trivial_rvalstruct& operator=(trivial_rvalstruct&&) = default;
  };
  static_assert(std::is_trivial<trivial_rvalstruct>::value, "");

  trivial_rvalstruct a[1], b[1];
  std::move_backward(a, a + 1, b + 1);
}
// { dg-prune-output "use of deleted" }
