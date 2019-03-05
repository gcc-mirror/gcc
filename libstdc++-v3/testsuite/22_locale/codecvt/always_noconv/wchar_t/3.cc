// { dg-require-namedlocale "en_US.ISO8859-1" }

// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

// 22.2.1.5 - Template class codecvt [lib.locale.codecvt]

#include <locale>
#include <testsuite_hooks.h>

// Required instantiation
// codecvt<wchar_t, char, mbstate_t>
//
// Baseline test in ISO8859-1 locale
void test03()
{
  using namespace std;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;

  locale loc = locale(ISO_8859(1,en_US));
  locale::global(loc);
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  VERIFY( !cvt->always_noconv() );
}

int main ()
{
  test03();
  return 0;
}
