// Copyright (C) 2000-2024 Free Software Foundation, Inc.
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

#include <cstdio>
#include <fstream>
#include <cstring>
#include <ext/stdio_filebuf.h>
#include <testsuite_hooks.h>

void test1()
{
  using namespace std;

  FILE* file = fopen("tmp_10063-1", "w");
  putc('0', file);
  putc('1', file);
  {
    __gnu_cxx::stdio_filebuf<char> sbuf(file, ios_base::out);
    sbuf.sputc('2');
    sbuf.sputc('3');
  }
  putc('4', file);	
  fclose(file);

  filebuf fbuf;
  fbuf.open("tmp_10063-1", ios_base::in);	
  char buf[10];
  streamsize n = fbuf.sgetn(buf, sizeof(buf));		
  fbuf.close();

  VERIFY(n == 5);
  VERIFY(!memcmp(buf, "01234", 5)); 
}

int main()
{
  test1();
  return 0;
}
