// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <functional>
#include <cstddef>

// PR libstdc++/83607

using std::boyer_moore_searcher;
using std::boyer_moore_horspool_searcher;
using std::byte;
using std::hash;
using std::equal_to;

void
test01()
{
  constexpr auto expected = sizeof(boyer_moore_searcher<const char*>);
  static_assert(sizeof(boyer_moore_searcher<const long*>) != expected);
  using T1 = boyer_moore_searcher<char*, hash<char>, equal_to<char>>;
  static_assert(sizeof(T1) == expected);
  using T2 = boyer_moore_searcher<byte*>;
  static_assert(sizeof(T2) == expected);
  using T3 = boyer_moore_searcher<const byte*>;
  static_assert(sizeof(T3) == expected);
  using T4 = boyer_moore_searcher<const byte*, hash<byte>, equal_to<byte>>;
  static_assert(sizeof(T4) == expected);
}

void
test02()
{
  constexpr auto expected = sizeof(boyer_moore_horspool_searcher<const char*>);
  static_assert(sizeof(boyer_moore_horspool_searcher<const long*>) != expected);
  using T1 = boyer_moore_horspool_searcher<char*, hash<char>, equal_to<char>>;
  static_assert(sizeof(T1) == expected);
  using T2 = boyer_moore_horspool_searcher<byte*>;
  static_assert(sizeof(T2) == expected);
  using T3 = boyer_moore_horspool_searcher<const byte*>;
  static_assert(sizeof(T3) == expected);
  using T4
    = boyer_moore_horspool_searcher<const byte*, hash<byte>, equal_to<byte>>;
  static_assert(sizeof(T4) == expected);
}
