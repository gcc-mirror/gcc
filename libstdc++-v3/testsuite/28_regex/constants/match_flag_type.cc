// { dg-do compile { target c++11 } }
// { dg-timeout-factor 2 }
//
// 2009-06-17  Stephen M. Webb  <stephen.webb@xandros.com>
//
// Copyright (C) 2009-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 28.5.2 [re.matchflag]

#include <regex>
#include <testsuite_common_types.h>

void
test01()
{
  std::regex_constants::match_flag_type flag = std::regex_constants::match_default;

  flag |= std::regex_constants::match_not_bol;
  flag |= std::regex_constants::match_not_eol;
  flag |= std::regex_constants::match_not_bow;
  flag |= std::regex_constants::match_not_eow;
  flag |= std::regex_constants::match_any;
  flag |= std::regex_constants::match_not_null;
  flag |= std::regex_constants::match_continuous;
  flag |= std::regex_constants::match_prev_avail;
  flag |= std::regex_constants::format_default;
  flag |= std::regex_constants::format_sed;
  flag |= std::regex_constants::format_no_copy;
  flag |= std::regex_constants::format_first_only;
}

#if __cplusplus >= 201402L
static_assert(
    __gnu_test::test_bitmask_values( {
      std::regex_constants::match_not_bol,
      std::regex_constants::match_not_eol,
      std::regex_constants::match_not_bow,
      std::regex_constants::match_not_eow,
      std::regex_constants::match_any,
      std::regex_constants::match_not_null,
      std::regex_constants::match_continuous,
      std::regex_constants::match_prev_avail,
      std::regex_constants::format_sed,
      std::regex_constants::format_no_copy,
      std::regex_constants::format_first_only
    },
    {
      std::regex_constants::match_default,
      std::regex_constants::format_default
    }),
    "std::regex_constants::match_flag_type bitmask elements are distinct" );
#endif

int main()
{
  test01();
  return 0;
}
