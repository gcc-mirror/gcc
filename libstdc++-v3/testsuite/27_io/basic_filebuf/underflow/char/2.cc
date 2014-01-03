// 2003-06-25 Paolo Carlini <pcarlini@unitus.it>

// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  filebuf fb_out, fb_in_out;
  
  fb_out.open("tmp_underflow.tst", ios::out);
  fb_out.sputc('S');
  fb_out.sputc('T');
  fb_out.close();

  fb_in_out.open("tmp_underflow.tst", ios::in | ios::out);
  while (fb_in_out.sbumpc() != filebuf::traits_type::eof());

  VERIFY( fb_in_out.sputc('x') == 'x' );
  fb_in_out.close();
}

int main()
{
  test01();
}
