// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

// LWG 2499
// operator>>(basic_istream&, CharT*) makes it hard to avoid buffer overflows

#include <istream>

void
test01(std::istream& in, char* pc, signed char* sc, unsigned char* uc)
{
  in >> pc; // { dg-error "no match" }
  in >> sc; // { dg-error "no match" }
  in >> uc; // { dg-error "no match" }
}

struct CT : std::char_traits<char> { };

void
test02(std::basic_istream<char, CT>& in, char* pc, signed char* sc,
       unsigned char* uc)
{
  in >> pc; // { dg-error "no match" }
  in >> sc; // { dg-error "no match" }
  in >> uc; // { dg-error "no match" }
}

// { dg-excess-errors "" }
