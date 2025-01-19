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

#include "codecvt_unicode.h"

#include <codecvt>

using namespace std;

void
test_utf8_utf32_codecvts ()
{
#if __SIZEOF_WCHAR_T__ == 4
  codecvt_utf8<wchar_t> cvt;
  test_utf8_utf32_cvt (cvt);
#endif
}

void
test_utf8_utf16_codecvts ()
{
#if __SIZEOF_WCHAR_T__ >= 2
  codecvt_utf8_utf16<wchar_t> cvt;
  test_utf8_utf16_cvt (cvt);
#endif
}

void
test_utf8_ucs2_codecvts ()
{
#if __SIZEOF_WCHAR_T__ == 2
  codecvt_utf8<wchar_t> cvt;
  test_utf8_ucs2_cvt (cvt);
#endif
}

void
test_utf16_utf32_codecvts ()
{
#if __SIZEOF_WCHAR_T__ == 4
  codecvt_utf16<wchar_t> cvt3;
  test_utf16_utf32_cvt (cvt3, utf16_big_endian);

  codecvt_utf16<wchar_t, 0x10FFFF, codecvt_mode::little_endian> cvt4;
  test_utf16_utf32_cvt (cvt4, utf16_little_endian);
#endif
}

void
test_utf16_ucs2_codecvts ()
{
#if __SIZEOF_WCHAR_T__ == 2
  codecvt_utf16<wchar_t> cvt3;
  test_utf16_ucs2_cvt (cvt3, utf16_big_endian);

  codecvt_utf16<wchar_t, 0x10FFFF, codecvt_mode::little_endian> cvt4;
  test_utf16_ucs2_cvt (cvt4, utf16_little_endian);
#endif
}

int
main ()
{
  test_utf8_utf32_codecvts ();
  test_utf8_utf16_codecvts ();
  test_utf8_ucs2_codecvts ();
  test_utf16_utf32_codecvts ();
  test_utf16_ucs2_codecvts ();
}
