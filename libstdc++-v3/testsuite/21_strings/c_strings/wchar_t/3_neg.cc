// { dg-do compile { target correct_iso_cpp_string_wchar_protos } }
// { dg-options "-O2" }

// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

#include <cwchar>

const wchar_t *cw1, *cw2;
wchar_t *w1, *w2;

void
test01 ()
{
  w1 = wmemchr (w2, L'/', 3);
  w1 = wcschr (w2, L'/');
  w1 = wcspbrk (w2, L"abc");
  w1 = wcsrchr (w2, L'c');
  w1 = wcsstr (w2, L"abc");

  cw1 = wmemchr (w2, L'/', 3);
  cw1 = wcschr (w2, L'/');
  cw1 = wcspbrk (w2, L"abc");
  cw1 = wcsrchr (w2, L'c');
  cw1 = wcsstr (w2, L"abc");

  w1 = wmemchr (cw2, L'/', 3);		// { dg-error "invalid conversion" }
  w1 = wcschr (cw2, L'/');		// { dg-error "invalid conversion" }
  w1 = wcsrchr (cw2, L'c');		// { dg-error "invalid conversion" }
  w1 = wcspbrk (cw2, L"abc");		// { dg-error "invalid conversion" }
  w1 = wcsstr (cw2, L"abc");		// { dg-error "invalid conversion" }

  cw1 = wmemchr (cw2, L'/', 3);
  cw1 = wcschr (cw2, L'/');
  cw1 = wcspbrk (cw2, L"abc");
  cw1 = wcsrchr (cw2, L'c');
  cw1 = wcsstr (cw2, L"abc");
}
