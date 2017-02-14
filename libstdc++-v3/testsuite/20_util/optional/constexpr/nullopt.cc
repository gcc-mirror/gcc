// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
  // [20.5.6] Disengaged state indicator
  static_assert( std::is_same<decltype(std::nullopt), const std::nullopt_t>(), "" );
  static_assert( std::is_empty<std::nullopt_t>(), "" );
  static_assert( std::is_literal_type<std::nullopt_t>(), "" );
  static_assert( !std::is_default_constructible<std::nullopt_t>(), "" );

  {
    constexpr std::optional<int> o = std::nullopt;
    static_assert( !o, "" );
  }

  {
    constexpr std::optional<int> o = { std::nullopt };
    static_assert( !o, "" );
  }

  {
    constexpr std::optional<int> o { std::nullopt };
    static_assert( !o, "" );
  }
}
