// { dg-do compile { target c++11 } }
//
// 2009-06-17  Stephen M. Webb  <stephen.webb@xandros.com>
//
// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

// 28.5.3 

#include <regex>

void
test01()
{
  std::regex_constants::error_type err __attribute__((unused));

  err = std::regex_constants::error_collate;
  err = std::regex_constants::error_ctype;
  err = std::regex_constants::error_escape;
  err = std::regex_constants::error_backref;
  err = std::regex_constants::error_brack;
  err = std::regex_constants::error_paren;
  err = std::regex_constants::error_brace;
  err = std::regex_constants::error_badbrace;
  err = std::regex_constants::error_range;
  err = std::regex_constants::error_space;
  err = std::regex_constants::error_badrepeat;
  err = std::regex_constants::error_complexity;
  err = std::regex_constants::error_stack;
}

int main()
{
  test01();
  return 0;
}
