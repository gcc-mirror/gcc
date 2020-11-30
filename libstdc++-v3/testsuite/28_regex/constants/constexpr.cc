// { dg-do compile { target c++11 } }
// { dg-timeout-factor 2 }
//
// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// 28.5.4

#include <regex>

// libstdc++/65420
void
test01()
{
  const std::regex_constants::syntax_option_type* option __attribute__((unused));
  option = &std::regex_constants::icase;
  option = &std::regex_constants::nosubs;
  option = &std::regex_constants::optimize;
  option = &std::regex_constants::collate;
  option = &std::regex_constants::ECMAScript;
  option = &std::regex_constants::basic;
  option = &std::regex_constants::extended;
  option = &std::regex_constants::awk;
  option = &std::regex_constants::grep;
  option = &std::regex_constants::egrep;

  const std::regex_constants::match_flag_type* flag __attribute__((unused));
  flag = &std::regex_constants::match_not_bol;
  flag = &std::regex_constants::match_not_eol;
  flag = &std::regex_constants::match_not_bow;
  flag = &std::regex_constants::match_not_eow;
  flag = &std::regex_constants::match_any;
  flag = &std::regex_constants::match_not_null;
  flag = &std::regex_constants::match_continuous;
  flag = &std::regex_constants::match_prev_avail;
  flag = &std::regex_constants::format_default;
  flag = &std::regex_constants::format_sed;
  flag = &std::regex_constants::format_no_copy;
  flag = &std::regex_constants::format_first_only;
}

int main()
{
  test01();
  return 0;
}
