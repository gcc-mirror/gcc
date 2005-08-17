// { dg-require-namedlocale "" }

// 2003-05-01  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <iostream>
#include <cstdio>
#include <testsuite_hooks.h>

// Test handling of UTF-8 in wcin
void test10()
{
  using namespace std;
  
  bool test __attribute__((unused)) = true;
  const char* name = "tmp_10";

  locale loc(locale("se_NO.UTF-8"));
  locale::global(loc);
  wcin.imbue(loc);
  wcout.imbue(loc);

  const char* e_lit =
    "\x1\x2\x3\x4\x5\x6\x7\x8\x9\xa\xb\xc\xd\xe\xf\x10\x11\x12\x13"
    "\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20!\"#$%&"
    "'()*+,-./0123456789:;<=>?@}~\x7f\xc2\x80\xc2\x81\xc2\x82\xc2"
    "\x83\xc2\x84\xc2\x85\xc2\x86\xc2\x87\xc2\x88\xc2\x89\xc2\x8a"
    "\xc2\x8b\xc2\x8c\xc2\x8d\xc2\x8e\xc2\x8f\xc2\x90\xc2\x91\xc2"
    "\x92\xc2\x93\xc2\x94\xc2\x95\xc2\x96\xc2\x97\xc2\x98\xc2\x99"
    "\xc2\x9a\xc2\x9b\xc2\x9c\xc3\xba\xc3\xbb\xc3\xbc\xc3\xbd\xc3"
    "\xbe\xc3\xbf\xc4\x80\xc4\x81\xc4\x82\xc4\x83\xc4\x84\xc4\x85"
    "\xc4\x86\xc4\x87\xc4\x88\xc4\x89\xc4\x8a\xc4\x8b\xc4\x8c\xc4"
    "\x8d\xc4\x8e\xc4\x8f\xc4\x90\xc4\x91\xc4\x92\xc4\x93\xc4\x94"
    "\xc4\x95\xc4\x96\xc4\x97\xc4\x98\xc4\x99\xdf\xb8\xdf\xb9\xdf"
    "\xba\xdf\xbb\xdf\xbc\xdf\xbd\xdf\xbe\xdf\xbf\xe0\xa0\x80\xe0"
    "\xa0\x81\xe0\xa0\x82\xe0\xa0\x83\xe0\xa0\x84\xe0\xa0\x85\xe0"
    "\xa0\x86\xe0\xa0\x87\xe0\xa0\x88\xe0\xa0\x89\xe0\xa0\x8a\xe0"
    "\xa0\x8b\xe0\xa0\x8c\xe0\xa0\x8d\xe0\xa0\x8e\xe0\xa0\x8f\xe0"
    "\xa0\x90\xe0\xa0\x91\xe0\xa0\x92\xe0\xa0\x93\xe0\xa0\x94\xe0"
    "\xa0\x95\xe0\xa0\x96\xe0\xa0\x97\x1\x2\x4\x8\x10\x20@\xc2\x80"
    "\xc4\x80\xc8\x80\xd0\x80\xe0\xa0\x80\xe1\x80\x80\xe2\x80\x80"
    "\xe4\x80\x80\xe8\x80\x80\xf0\x90\x80\x80\xf0\xa0\x80\x80\xf1"
    "\x80\x80\x80\xf2\x80\x80\x80\xf4\x80\x80\x80\xf8\x88\x80\x80"
    "\x80\xf8\x90\x80\x80\x80\xf8\xa0\x80\x80\x80\xf9\x80\x80\x80"
    "\x80\xfa\x80\x80\x80\x80\xfc\x84\x80\x80\x80\x80\xfc\x88\x80"
    "\x80\x80\x80\xfc\x90\x80\x80\x80\x80\xfc\xa0\x80\x80\x80\x80"
    "\xfd\x80\x80\x80\x80\x80";
  size_t e_size = strlen(e_lit);

  const wchar_t i_lit[] = {
    0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc,
    0xd, 0xe, 0xf, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, L'!',
    L'"', L'#', L'$', L'%', L'&', L'\'', L'(', L')', L'*', L'+',
    L',', L'-', L'.', L'/', L'0', L'1', L'2', L'3', L'4', L'5',
    L'6', L'7', L'8', L'9', L':', L';', L'<', L'=', L'>', L'?',
    L'@', L'}', L'~', 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85,
    0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99,
    0x9a, 0x9b, 0x9c, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff, 0x100,
    0x101, 0x102, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108, 0x109,
    0x10a, 0x10b, 0x10c, 0x10d, 0x10e, 0x10f, 0x110, 0x111, 0x112,
    0x113, 0x114, 0x115, 0x116, 0x117, 0x118, 0x119, 0x7f8, 0x7f9,
    0x7fa, 0x7fb, 0x7fc, 0x7fd, 0x7fe, 0x7ff, 0x800, 0x801, 0x802,
    0x803, 0x804, 0x805, 0x806, 0x807, 0x808, 0x809, 0x80a, 0x80b,
    0x80c, 0x80d, 0x80e, 0x80f, 0x810, 0x811, 0x812, 0x813, 0x814,
    0x815, 0x816, 0x817, 0x1, 0x2, 0x4, 0x8, 0x10, 0x20, L'@',
    0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000,
    0x10000, 0x20000, 0x40000, 0x80000, 0x100000, 0x200000, 0x400000,
    0x800000, 0x1000000, 0x2000000, 0x4000000, 0x8000000, 0x10000000,
    0x20000000, 0x40000000, 0x0
  };
  size_t i_size = wcslen(i_lit);

  FILE* file = fopen(name, "w");
  size_t n = fwrite(e_lit, 1, e_size, file);
  VERIFY( n == e_size );
  fclose(file);

  freopen(name, "r", stdin);
  
  wchar_t* wbuf = new wchar_t[i_size + 10];
  wcin.read(wbuf, i_size + 10);
  n = wcin.gcount();
  VERIFY( n == i_size );
  VERIFY( !wmemcmp(wbuf, i_lit, i_size) );
  VERIFY( wcin.eof() );
  VERIFY( wcin.fail() );
  VERIFY( !wcin.bad() );
  delete[] wbuf;
}

int main()
{
  test10();
  return 0;
}
