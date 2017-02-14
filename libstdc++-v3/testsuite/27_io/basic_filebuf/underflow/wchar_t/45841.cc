// Copyright (C) 2010-2017 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  wfilebuf fb_in_out;

  fb_in_out.open("tmp_underflow.tst", ios::in | ios::out | ios::trunc);

  VERIFY( fb_in_out.sputc(L'x') == L'x' );
  VERIFY( fb_in_out.sgetc() == wfilebuf::traits_type::eof() );
  fb_in_out.close();
}

int main()
{
  test01();
  return 0;
}
