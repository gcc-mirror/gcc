// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/string_view>

struct constexpr_char_traits : std::char_traits<char>
{
  static constexpr size_t
  length(const char* val)
  {
    size_t res = 0;
    for (; val[res] != '\0'; ++res)
      ;
    return res;
  }

  static constexpr int
  compare(const char* lhs, const char* rhs, std::size_t count)
  {
    for (size_t pos = 0; pos < count; ++pos)
    {
      if (lhs[pos] != rhs[pos])
        return lhs[pos] - rhs[pos];
    }
    return 0;
  }

  static constexpr const char*
  find(const char* p, std::size_t n, char c)
  {
    for (size_t pos = 0; pos < n; ++pos)
      if (p[pos] == c)
	return p + pos;
    return nullptr;
  }
};

using string_view
  = std::experimental::basic_string_view<char, constexpr_char_traits>;

constexpr
string_view get()
{
    string_view res = "x::";
    string_view start_pattern = "x";
    res = res.substr(res.find(start_pattern) + start_pattern.size());
    res = res.substr(0, res.find_first_of(";]"));
    res = res.substr(res.rfind("::"));
    return res;
}

static_assert( get() == get() );
