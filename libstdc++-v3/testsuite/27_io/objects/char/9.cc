// 2003-05-01  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2016 Free Software Foundation, Inc.
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
 
#include <iostream>
#include <cstdio>
#include <testsuite_hooks.h>

void test09()
{
  using namespace std;

  const char* name = "tmp_09";

  FILE* fout = fopen(name, "w");
  fputs("abc\n", fout);
  fclose(fout);

  VERIFY( freopen(name, "r", stdin) );
 
  // basic_streambuf::showmanyc returns 0.
  VERIFY( 0 == std::cin.rdbuf()->in_avail() );
}

int 
main()
{
  test09();
  return 0;
}
