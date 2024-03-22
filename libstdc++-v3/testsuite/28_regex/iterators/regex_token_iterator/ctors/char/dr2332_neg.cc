// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// { dg-timeout-factor 2 }

#include <regex>

void
test01()
{
  using iter_type = std::regex_token_iterator<const char*>;
  const char* s = "";

  iter_type(s, s, std::regex{});	// { dg-error "deleted" }

  std::vector<int> v;
  iter_type(s, s, std::regex{}, v);	// { dg-error "deleted" }

  std::initializer_list<int> il = {1};
  iter_type(s, s, std::regex{}, il);	// { dg-error "deleted" }

  const int i[2] = { };
  iter_type(s, s, std::regex{}, i);	// { dg-error "deleted" }
}
