// { dg-do compile }
// { dg-require-c-std "" }
// { dg-require-swprintf "" }

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

namespace gnu
{
  using std::btowc;
  using std::fgetwc;
  using std::fgetws;
  using std::fputwc;
  using std::fputws;
  using std::fwide;
  using std::fwprintf;
  using std::fwscanf;
  using std::getwc;
  using std::getwchar;
  using std::mbrlen;
  using std::mbrtowc;
  using std::mbsinit;
  using std::mbsrtowcs;
  using std::putwc;
  using std::putwchar;
  using std::swprintf;
  using std::swscanf;
  using std::ungetwc;
  using std::vfwprintf;
  using std::vswprintf;
  using std::vwprintf;
  using std::wcrtomb;
  using std::wcscat;
  using std::wcschr;
  using std::wcscmp;
  using std::wcscoll;
  using std::wcscpy;
  using std::wcscmp;
  using std::wcsftime;
  using std::wcslen;
  using std::wcsncat;
  using std::wcsncmp;
  using std::wcsncpy;
  using std::wcspbrk;
  using std::wcsrchr;
  using std::wcsrtombs;
  using std::wcsspn;
  using std::wcsstr;
  using std::wcstod;
  using std::wcstok;
  using std::wcstol;
  using std::wcstoul;
  using std::wcsxfrm;
  using std::wctob;
  using std::wmemchr;
  using std::wmemcmp;
  using std::wmemcpy;
  using std::wmemmove;
  using std::wmemset;
  using std::wprintf;
  using std::wscanf;
}
