// { dg-do compile }

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

#include <climits>

namespace gnu
{
  // char
  char c1 = CHAR_BIT;
  char c2 = CHAR_MAX;
  char c3 = CHAR_MIN;

  // int
  int i2 = INT_MAX;
  int i3 = INT_MIN;

  // short
  short s2 = SHRT_MAX;
  short s3 = SHRT_MIN;

  // long
  long l2 = LONG_MAX;
  long l3 = LONG_MIN;

  unsigned long mb = MB_LEN_MAX;

  // signed char
  signed char sc1 = SCHAR_MIN;
  signed char sc2 = SCHAR_MAX;

  // unsigned
  unsigned int ui = UINT_MAX;
  unsigned short us = USHRT_MAX;
  unsigned long ul = ULONG_MAX;

}
