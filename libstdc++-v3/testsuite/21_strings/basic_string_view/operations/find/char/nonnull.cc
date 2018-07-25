// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -Wnonnull" }
// { dg-do compile { target c++17 } }

#include <string_view>

int
test01()
{
  std::string_view s = "abcd";
  return s.find((const char*)nullptr);		// { dg-warning "null arg" }
  return s.find((const char*)nullptr, 1);	// { dg-warning "null arg" }
  return s.find_first_of((const char*)nullptr);	// { dg-warning "null arg" }
  return s.find_first_of((const char*)nullptr, 1); // { dg-warning "null arg" }
  return s.find_first_not_of((const char*)nullptr); // { dg-warning "null arg" }
  return s.find_first_not_of((const char*)nullptr, 1); // { dg-warning "null arg" }
}
