// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  // PR libstdc++83598
  std::regex r1(".", std::regex_constants::syntax_option_type{});
  VERIFY(r1.flags() == std::regex_constants::syntax_option_type{});
  std::regex r2(".", std::regex_constants::icase);
  VERIFY(r2.flags() == std::regex_constants::icase);
}

int
main()
{
  test01();
}
