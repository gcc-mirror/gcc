// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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
  // [20.5.4.1] Constructors

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::optional<long> o { i };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::optional<long> o = i;
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::optional<long> o = { i };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::optional<long> o { std::move(i) };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::optional<long> o = std::move(i);
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }

  {
    constexpr long i = 0x1234ABCD;
    constexpr std::optional<long> o = { std::move(i) };
    static_assert( o, "" );
    static_assert( *o == 0x1234ABCD, "" );
  }
  {
    constexpr std::optional<long> o = 42;
    constexpr std::optional<long> o2{o};
    constexpr std::optional<long> o3(o);
    constexpr std::optional<long> o4 = o;
    constexpr std::optional<long> o5;
    constexpr std::optional<long> o6{o5};
    constexpr std::optional<long> o7(o5);
    constexpr std::optional<long> o8 = o5;
    constexpr std::optional<long> o9{std::move(o)};
    constexpr std::optional<long> o10(std::move(o));
    constexpr std::optional<long> o11 = std::move(o);
    constexpr std::optional<long> o12;
    constexpr std::optional<long> o13{std::move(o5)};
    constexpr std::optional<long> o14(std::move(o5));
    constexpr std::optional<long> o15 = std::move(o5);
  }
}
