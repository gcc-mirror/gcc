// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

// libstdc++/59427
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  filebuf a_f;
  VERIFY( a_f.open("tmp_59427_sputc", ios_base::in | ios_base::app) );
  VERIFY( 'a' == a_f.sputc('a')  );
  VERIFY( a_f.close() );
}

int main()
{
  test01();
  return 0;
}
