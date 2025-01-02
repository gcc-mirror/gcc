// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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
  FILE* f = std::fopen("79820.txt", "w");
  errno = 127;
  __gnu_cxx::stdio_filebuf<char> b(f, std::ios::out, BUFSIZ);
  VERIFY(errno == 127); // PR libstdc++/79820
  b.close();
  std::fclose(f);
}

int
main()
{
  test01();
}
