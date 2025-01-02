// 2003-05-01 Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

#include <ext/stdio_sync_filebuf.h>
#include <cstring>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  const char* c_lit = "black pearl jasmine tea";
  unsigned size = strlen(c_lit);
  const char* name = "stdiobuf-1.txt";

  FILE* fout = fopen(name, "w");
  VERIFY( fwrite(c_lit, 1, size, fout) == size );
  fclose(fout);

  FILE* fin = fopen(name, "r");
  __gnu_cxx::stdio_sync_filebuf<char> sbuf(fin);

  VERIFY( sbuf.sgetc() == c_lit[0] );
  VERIFY( getc(fin) == c_lit[0] );
  VERIFY( sbuf.sgetc() == c_lit[1] );
  VERIFY( sbuf.sbumpc() == c_lit[1] );
  VERIFY( ungetc('Z', fin) == 'Z' );
  VERIFY( sbuf.sbumpc() == 'Z' );
  VERIFY( getc(fin) == c_lit[2] );
  VERIFY( sbuf.sputbackc('X') == 'X' );
  VERIFY( getc(fin) == 'X' );

  char buf[5];
  memset(buf, 'x', 5);
  VERIFY( sbuf.sgetn(buf, 5) == 5 );
  VERIFY( !memcmp(buf, c_lit + 3, 5) );
  VERIFY( getc(fin) == c_lit[8] );

  fclose(fin);
}

int main ()
{
  test01();
  return 0;
}
