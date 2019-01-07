// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 }  }

// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

#include <vector>

int main()
{
  // [20.5.5] In-place construction
  {
    std::optional<int> o { std::in_place };
    VERIFY( o );
    VERIFY( *o == int() );

    static_assert( !std::is_convertible<std::in_place_t, std::optional<int>>(), "" );
  }

  {
    std::optional<int> o { std::in_place, 42 };
    VERIFY( o );
    VERIFY( *o == 42 );
  }

  {
    std::optional<std::vector<int>> o { std::in_place, 18, 4 };
    VERIFY( o );
    VERIFY( o->size() == 18 );
    VERIFY( (*o)[17] == 4 );
  }

  {
    std::optional<std::vector<int>> o { std::in_place, { 18, 4 } };
    VERIFY( o );
    VERIFY( o->size() == 2 );
    VERIFY( (*o)[0] == 18 );
  }

  {
    std::optional<std::vector<int>> o { std::in_place, { 18, 4 }, std::allocator<int> {} };
    VERIFY( o );
    VERIFY( o->size() == 2 );
    VERIFY( (*o)[0] == 18 );
  }
}
