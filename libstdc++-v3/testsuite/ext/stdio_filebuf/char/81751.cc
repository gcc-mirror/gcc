// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

#include <ext/stdio_filebuf.h>
#include <cstdio>
#include <cerrno>
#include <testsuite_hooks.h>

void
test01()
{
  FILE* out = std::fopen("81751.txt", "w");
  std::fwrite("Some words.", 1, 10, out);

  FILE* in1 = std::fopen("81751.txt", "r");
  __gnu_cxx::stdio_filebuf<char> buf1(in1, std::ios::in, BUFSIZ);
  int c = buf1.sgetc();
  VERIFY( c == std::char_traits<char>::eof() ); // PR libstdc++/81751

  std::fflush(out);
  FILE* in2 = std::fopen("81751.txt", "r");
  __gnu_cxx::stdio_filebuf<char> buf2(in2, std::ios::in, BUFSIZ);
  c = buf2.sgetc();
  VERIFY( c == 'S' );

  buf1.close();
  buf2.close();
  std::fclose(in1);
  std::fclose(in2);
  std::fclose(out);
}

int
main()
{
  test01();
}
