// { dg-do run { xfail *-*-* } }
// { dg-options "-O0" }
// { dg-require-debug-mode "" }

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

#include <string>

// libstdc++/21674
// NB: Should work without any inlining or optimizations (ie. -O0).
int main()
{
  typedef std::wstring string_type;
  string_type s;
  (void) s[1]; // abort
}
