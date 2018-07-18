// { dg-do compile { target c++14 } }

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

#include <experimental/optional>
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
  using O = std::experimental::optional<value_type>;

  constexpr value_type reference { 42, "forty-two" };

  {
    constexpr O o;
    static_assert( !(o == reference), "" );
    static_assert( !(reference == o), "" );
    static_assert( o != reference, "" );
    static_assert( reference != o, "" );
  }

  {
    constexpr O o { value_type { 11, "eleventy" } };
    static_assert( !(o == reference), "" );
    static_assert( !(reference == o), "" );
    static_assert( o != reference, "" );
    static_assert( reference != o, "" );
  }

  {
    constexpr O o { value_type { 42, "forty-two" } };
    static_assert( o == reference, "" );
    static_assert( reference == o, "" );
    static_assert( !(o != reference), "" );
    static_assert( !(reference != o), "" );
  }
}
