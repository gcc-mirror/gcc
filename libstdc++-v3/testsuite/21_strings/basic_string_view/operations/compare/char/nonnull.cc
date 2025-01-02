// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-options "-Wnonnull" }
// { dg-do compile { target c++17 } }

#include <string_view>

int
test01()
{
  std::string_view s = "abcd";
  return s.compare((const char*)nullptr);	// { dg-warning "\\\[-Wnonnull" }
  return s.compare(0, 2, (const char*)nullptr);	// { dg-warning "\\\[-Wnonnull" }
}

// Ignore additional diagnostic given with -Wsystem-headers:
// { dg-prune-output "argument 1 null where non-null expected" }
