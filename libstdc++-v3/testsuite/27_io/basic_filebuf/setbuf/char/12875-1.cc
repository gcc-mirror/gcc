// Copyright (C) 2003 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <cstdio>
#include <cstring>
#include <testsuite_hooks.h>

// libstdc++/12875
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

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
