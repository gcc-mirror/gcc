// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <functional>
#include <string_view>

const std::string_view
patt = "World";

constexpr std::string_view
greet = "Hello, Humongous World of Wonder!!!";

const std::wstring_view
wpatt = L"World";

constexpr std::wstring_view
wgreet = L"Hello, Humongous World of Wonder!!!";

constexpr bool
test_searcher()
{
  auto ok = true;

  const std::default_searcher search(patt.begin(), patt.end(),
				     std::equal_to<>());
  const auto find = search(greet.begin(), greet.end());

  const std::default_searcher wsearch(wpatt.begin(), wpatt.end(),
				      std::equal_to<>());
  const auto wfind = wsearch(wgreet.begin(), wgreet.end());

  return ok;
}

static_assert(test_searcher());
