// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

// 27.8.1.3 filebuf member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// various tests for filebuf::open() and filebuf::close() including
// the non-portable functionality in the libstdc++-v3 IO library

// { dg-require-fileio "" }

#include <fstream>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ext/stdio_filebuf.h>
#include <testsuite_hooks.h>

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it

void test_03()
{
  int first_fd = ::open(name_01, O_RDONLY);
  VERIFY( first_fd != -1 );
  FILE* first_file = ::fdopen(first_fd, "r");
  VERIFY( first_file );
  __gnu_cxx::stdio_filebuf<char> fb(first_file, std::ios_base::in);

  int second_fd = fb.fd();

  VERIFY( first_fd == second_fd );
}

int
main()
{
  test_03();
  return 0;
}


