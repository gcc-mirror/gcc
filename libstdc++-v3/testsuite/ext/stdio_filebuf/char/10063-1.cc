// Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation
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

#include <cstdio>
#include <fstream>
#include <ext/stdio_filebuf.h>
#include <testsuite_hooks.h>

void test1()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

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
