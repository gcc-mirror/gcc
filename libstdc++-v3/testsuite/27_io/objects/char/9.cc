// 2003-05-01  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }
 
#include <iostream>
#include <cstdio>
#include <testsuite_hooks.h>

void test09()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  const char* name = "tmp_09";

  FILE* fout = fopen(name, "w");
  fputs("abc\n", fout);
  fclose(fout);

  freopen(name, "r", stdin);
 
  // basic_streambuf::showmanyc returns 0.
  VERIFY( 0 == std::cin.rdbuf()->in_avail() );
}

int 
main()
{
  test09();
  return 0;
}
