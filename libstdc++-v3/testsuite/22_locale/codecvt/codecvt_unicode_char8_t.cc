// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
// { dg-require-cstdint "" }
// { dg-options "-fchar8_t" }

#include "codecvt_unicode.h"

using namespace std;

void
test_utf8_utf32_codecvts ()
{
  using codecvt_c32_c8 = codecvt<char32_t, char8_t, mbstate_t>;
  auto &loc_c = locale::classic ();
  VERIFY (has_facet<codecvt_c32_c8> (loc_c));

  auto &cvt = use_facet<codecvt_c32_c8> (loc_c);
  test_utf8_utf32_cvt (cvt);
}

void
test_utf8_utf16_codecvts ()
{
  using codecvt_c16_c8 = codecvt<char16_t, char8_t, mbstate_t>;
  auto &loc_c = locale::classic ();
  VERIFY (has_facet<codecvt_c16_c8> (loc_c));

  auto &cvt = use_facet<codecvt_c16_c8> (loc_c);
  test_utf8_utf16_cvt (cvt);
}

int
main ()
{
  test_utf8_utf32_codecvts ();
  test_utf8_utf16_codecvts ();
}
