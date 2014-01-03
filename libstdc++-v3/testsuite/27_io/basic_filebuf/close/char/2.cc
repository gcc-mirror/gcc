// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 27.8.1.3 filebuf member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// various tests for filebuf::open() and filebuf::close() including
// the non-portable functionality in the libstdc++-v3 IO library

#include <fstream>
#include <ext/stdio_filebuf.h>
#include <testsuite_hooks.h>

// Verify that std::filebuf doesn't close files that it didn't open
// when using the following std::filebuf ctor:
//
//      std::filebuf(__c_file_type*  __f,
//                   ios_base::openmode __mode,
//                   int_type  __s);
//
// Thanks to "George T. Talbot" <george@moberg.com> for uncovering
// this bug/situation. 

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it
const char name_02[] = "filebuf_virtuals-2.txt"; // empty file, need to create

void test_02()
{
  bool test __attribute__((unused)) = true;
  int close_num = 0;

  // read (ext)
  FILE* f2 = fopen(name_01, "r");
  VERIFY( f2 );
  if (f2)
  {
    __gnu_cxx::stdio_filebuf<char> fb(f2, std::ios_base::in, 512);
    close_num = fclose(f2);
  }
  VERIFY( close_num == 0 );

  // read (standard)
  FILE* f = fopen(name_01, "r");
  VERIFY( f );
  if (f)
  {
    std::ifstream ifstream1(name_01);
    VERIFY( ifstream1.is_open() );
    std::ios_base::iostate st01 = ifstream1.rdstate();
    VERIFY( st01 == std::ios_base::goodbit );
    close_num = fclose(f);
  }
  VERIFY( close_num == 0 );
}

int
main()
{
  test_02();
  return 0;
}


