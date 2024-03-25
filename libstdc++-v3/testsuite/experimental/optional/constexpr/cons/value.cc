// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

#include <experimental/optional>
#include <testsuite_hooks.h>

int main()
{
  // [20.5.4.1] Constructors

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::experimental::optional<long> o { i };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::experimental::optional<long> o = i;
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::experimental::optional<long> o = { i };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::experimental::optional<long> o { std::move(i) };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::experimental::optional<long> o = std::move(i);
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::experimental::optional<long> o = { std::move(i) };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }
}
