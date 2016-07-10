// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <optional>
#include <testsuite_hooks.h>

namespace ns
{
  struct value_type
  {
    int i;
    const char* s;
  };

  constexpr bool
  strcmp(const char* lhs, const char* rhs)
  {
    return *lhs == *rhs && (!*lhs || strcmp(lhs + 1, rhs + 1));
  }

  constexpr bool
  strrel(const char* lhs, const char* rhs)
  {
    return (*rhs && (!*lhs || (*lhs < *rhs)))
      || ((*lhs && *rhs && !(*rhs < *lhs)) && strrel(lhs + 1, rhs + 1));
  }

  constexpr bool
  operator==(value_type const& lhs, value_type const& rhs)
  { return (lhs.i == rhs.i) && strcmp(lhs.s, rhs.s); }

  constexpr bool
  operator!=(value_type const& lhs, value_type const& rhs)
  { return !(lhs == rhs); }

  constexpr bool
  operator<(value_type const& lhs, value_type const& rhs)
  { return (lhs.i < rhs.i) || (!(rhs.i < lhs.i) && strrel(lhs.s, rhs.s)); }

} // namespace ns

int main()
{
  using ns::value_type;
  using O = std::optional<value_type>;

  {
    constexpr O o, p;
    static_assert( !(o < p), "" );
    static_assert( !(o > p), "" );
    static_assert( o <= p, "" );
    static_assert( o >= p, "" );
  }

  {
    constexpr O o { value_type { 42, "forty-two" } }, p;
    static_assert( !(o < p), "" );
    static_assert( o > p, "" );
    static_assert( !(o <= p), "" );
    static_assert( o >= p, "" );
  }

  {
    constexpr O o, p { value_type { 42, "forty-two" } };
    static_assert( o < p, "" );
    static_assert( !(o > p), "" );
    static_assert( o <= p, "" );
    static_assert( !(o >= p), "" );
  }

  {
    constexpr O o { value_type { 11, "eleventy" } }, p { value_type { 42, "forty-two" } };
    static_assert( o < p, "" );
    static_assert( !(o > p), "" );
    static_assert( o <= p, "" );
    static_assert( !(o >= p), "" );
  }

  {
    constexpr O o { value_type { 42, "forty-two" } }, p { value_type { 11, "eleventy" } };
    static_assert( !(o < p), "" );
    static_assert( o > p, "" );
    static_assert( !(o <= p), "" );
    static_assert( o >= p, "" );
  }

  {
    constexpr O o { value_type { 42, "forty-two" } }, p { value_type { 42, "forty-two" } };
    static_assert( !(o < p), "" );
    static_assert( !(o > p), "" );
    static_assert( o <= p, "" );
    static_assert( o >= p, "" );
  }
}
