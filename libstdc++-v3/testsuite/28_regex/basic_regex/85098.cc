// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-options "-O0" }
// { dg-do link { target c++11 } }
// { dg-timeout-factor 2 }

#include <regex>

void f(const std::regex_constants::syntax_option_type&) { }

void
test01()
{
  f(std::regex::icase);
  f(std::regex::nosubs);
  f(std::regex::optimize);
  f(std::regex::collate);
  f(std::regex::ECMAScript);
  f(std::regex::basic);
  f(std::regex::extended);
  f(std::regex::awk);
  f(std::regex::grep);
  f(std::regex::egrep);
  // f(std::regex::multiline);
}

int
main()
{
  test01();
}
