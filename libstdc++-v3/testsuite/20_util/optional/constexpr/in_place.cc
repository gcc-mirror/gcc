// { dg-do compile { target c++17 }  }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>

int main()
{
  // [20.5.5] In-place construction
  {
    constexpr std::optional<int> o { std::in_place };
    static_assert( o, "" );
    static_assert( *o == int {}, "" );

    static_assert( !std::is_convertible<std::in_place_t, std::optional<int>>(), "" );
  }

  {
    constexpr std::optional<int> o { std::in_place, 42 };
    static_assert( o, "" );
    static_assert( *o == 42, "" );
  }
}
