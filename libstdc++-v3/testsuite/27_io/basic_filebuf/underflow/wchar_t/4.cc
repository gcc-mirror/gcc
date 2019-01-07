// { dg-require-namedlocale "se_NO.UTF-8" }

// 2003-09-04  Petur Runolfsson  <peturr02@ru.is>
// Adapted from 27_io/basic_filebuf/underflow/char/2.cc

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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  locale loc (locale("se_NO.UTF-8"));
  wfilebuf fb_out, fb_in_out;
  fb_out.pubimbue(loc);
  fb_in_out.pubimbue(loc);
  
  fb_out.open("tmp_underflow.tst", ios::out);
  fb_out.sputc(L'S');
  fb_out.sputc(L'T');
  fb_out.close();

  fb_in_out.open("tmp_underflow.tst", ios::in | ios::out);
  while (fb_in_out.sbumpc() != wfilebuf::traits_type::eof());

  VERIFY( fb_in_out.sputc(L'x') == L'x' );
  fb_in_out.close();
}

int main()
{
  test01();
  return 0;
}
