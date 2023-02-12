// 2003-04-12  Paolo Carlini  <pcarlini at unitus dot it>

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

// stdio_filebuf.h

// { dg-require-fileio "" }

#include <ext/stdio_filebuf.h>
#include <cstdio>
#include <fstream>
#include <cstring>
#include <testsuite_hooks.h>

// Small stack-based buffers (i.e., using _M_unbuf) were not flushed
// out by _M_really_overflow upon overflow.
void test01()
{
  using namespace std;

  const char* name = "tmp_file1";
  FILE* file = fopen(name, "w");
  {
    using namespace __gnu_cxx;
    stdio_filebuf<char> sbuf(file, ios_base::out, 2); 
    sbuf.sputc('T');
    sbuf.sputc('S');
    sbuf.sputc('P');
  }
  fclose(file);

  filebuf fbuf;
  fbuf.open(name, ios_base::in);
  char buf[10];
  streamsize n = fbuf.sgetn(buf, sizeof(buf));	
  fbuf.close();
  
  VERIFY( n == 3 );
  VERIFY( !memcmp(buf, "TSP", 3) );
}

int main()
{
  test01();
}
