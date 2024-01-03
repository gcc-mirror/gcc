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

// The ARM simulator does not provide support for "fstat", which
// causes "sbumpc" to return an incorrect value.
// { dg-do run { xfail arm*-*-elf arm*-*-eabi } }

// { dg-require-fileio "" }

#include <cstdio>
#include <fstream>
#include <ext/stdio_filebuf.h>
#include <testsuite_hooks.h>

void test2()
{
  using namespace std;

  filebuf fbuf;
  fbuf.open("tmp_10063-2", ios_base::out | ios_base::trunc);
  fbuf.sputn("01234", 5);
  fbuf.close();

  FILE* file = fopen("tmp_10063-2", "r");
  setbuf(file, 0);
  int c = getc(file);
  VERIFY(c == '0');
  c = getc(file);
  VERIFY(c == '1');
  {
    __gnu_cxx::stdio_filebuf<char> sbuf(file, ios_base::in);
    c = sbuf.sbumpc();
    VERIFY(c == '2');
    c = sbuf.sbumpc();
    VERIFY(c == '3');
    c = sbuf.sbumpc();
    VERIFY(c == '4');
    c = sbuf.sgetc();
    VERIFY(c == EOF);
  }
  fclose(file);
}

int main()
{
  test2();
  return 0;
}
