// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include "codecvt_unicode.h"

#include <codecvt>

using namespace std;

void
test_utf8_utf32_codecvts ()
{
  using codecvt_c32 = codecvt<char32_t, char, mbstate_t>;
  auto loc_c = locale::classic ();
  VERIFY (has_facet<codecvt_c32> (loc_c));

  auto &cvt = use_facet<codecvt_c32> (loc_c);
  test_utf8_utf32_codecvts (cvt);

  codecvt_utf8<char32_t> cvt2;
  test_utf8_utf32_codecvts (cvt2);
}

void
test_utf8_utf16_codecvts ()
{
  using codecvt_c16 = codecvt<char16_t, char, mbstate_t>;
  auto loc_c = locale::classic ();
  VERIFY (has_facet<codecvt_c16> (loc_c));

  auto &cvt = use_facet<codecvt_c16> (loc_c);
  test_utf8_utf16_cvts (cvt);

  codecvt_utf8_utf16<char16_t> cvt2;
  test_utf8_utf16_cvts (cvt2);

  codecvt_utf8_utf16<char32_t> cvt3;
  test_utf8_utf16_cvts (cvt3);
}

void
test_utf8_ucs2_codecvts ()
{
  codecvt_utf8<char16_t> cvt;
  test_utf8_ucs2_cvts (cvt);
}

int
main ()
{
  test_utf8_utf32_codecvts ();
  test_utf8_utf16_codecvts ();
  test_utf8_ucs2_codecvts ();
}
