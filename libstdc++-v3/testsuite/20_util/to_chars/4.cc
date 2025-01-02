// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// { dg-do link { target c++17 } }
// { dg-require-effective-target ieee_floats }
// { dg-require-static-libstdcxx }
// { dg-additional-options "-static-libstdc++" }

// Verify the Ryu symbol generic_to_chars doesn't inadvertently leak into
// libstdc++.a.  If it did, this test would fail at link time with a multiple
// definition error.

#include <charconv>

extern "C" void generic_to_chars(void) { }

int
main()
{
  char x[64];
  std::to_chars(x, x+64, 42.L, std::chars_format::scientific);
}
