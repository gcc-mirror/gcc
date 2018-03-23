// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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

// 27.4.3 fpos

// { dg-do compile }

#include <ios>

void test04()
{
  std::streampos pos;
  long n;

  // Implicit conversion
  n = pos; // { dg-error "cannot convert" "" { xfail *-*-* } }

  // Explicit conversion
  n = static_cast<long>(pos); // { dg-error "invalid static_cast" "" { xfail *-*-* } }

  n = n; // Suppress unused warning.
}

int main()
{
  test04();
  return 0;
}
