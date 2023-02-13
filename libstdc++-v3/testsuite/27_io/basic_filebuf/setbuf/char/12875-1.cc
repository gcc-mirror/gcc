// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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
#include <cstdio>
#include <cstring>
#include <testsuite_hooks.h>

// libstdc++/12875
void test01()
{
  using namespace std;

  const char* name = "tmp_setbuf4";
  static char buf[1024];
  
  FILE* out = fopen(name, "w");
  fputs("Hello, world", out);
  fclose(out);
  
  filebuf in;
  in.open(name, ios_base::in);
  char str[256];
  streamsize r = in.sgetn(str, 6);
  VERIFY( r == 6 );
  VERIFY( !memcmp(str, "Hello,", 6) );
  in.pubsetbuf(buf, 1024);
  r = in.sgetn(str, 6);
  VERIFY( r == 6 );
  VERIFY( !memcmp(str, " world", 6) );
  in.close();
}

// libstdc++/12875
int main()
{
  test01();
  return 0;
}
