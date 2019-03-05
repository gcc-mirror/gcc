// { dg-do run { target c++14 } }

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

#include <experimental/optional>
#include <testsuite_hooks.h>

#include <tuple>
#include <string>

namespace ns
{
  struct value_type
  {
    int i;
    std::string s;
  };

  bool
  operator==(value_type const& lhs, value_type const& rhs)
  { return std::tie(lhs.i, lhs.s) == std::tie(rhs.i, rhs.s); }

  bool
  operator<(value_type const& lhs, value_type const& rhs)
  { return std::tie(lhs.i, lhs.s) < std::tie(rhs.i, rhs.s); }

} // namespace ns

int main()
{
  using ns::value_type;
  using O = std::experimental::optional<value_type>;

  {
    O o, p;
    VERIFY( !(o < p) );
    VERIFY( !(o > p) );
    VERIFY( o <= p );
    VERIFY( o >= p );
  }

  {
    O o { value_type { 42, "forty-two" } }, p;
    VERIFY( !(o < p) );
    VERIFY( o > p );
    VERIFY( !(o <= p) );
    VERIFY( o >= p );
  }

  {
    O o, p { value_type { 42, "forty-two" } };
    VERIFY( o < p );
    VERIFY( !(o > p) );
    VERIFY( o <= p );
    VERIFY( !(o >= p) );
  }

  {
    O o { value_type { 11, "eleventy" } }, p { value_type { 42, "forty-two" } };
    VERIFY( o < p );
    VERIFY( !(o > p) );
    VERIFY( o <= p );
    VERIFY( !(o >= p) );
  }

  {
    O o { value_type { 42, "forty-two" } }, p { value_type { 11, "eleventy" } };
    VERIFY( !(o < p) );
    VERIFY( o > p );
    VERIFY( !(o <= p) );
    VERIFY( o >= p );
  }

  {
    O o { value_type { 42, "forty-two" } }, p { value_type { 42, "forty-two" } };
    VERIFY( !(o < p) );
    VERIFY( !(o > p) );
    VERIFY( o <= p );
    VERIFY( o >= p );
  }
}
