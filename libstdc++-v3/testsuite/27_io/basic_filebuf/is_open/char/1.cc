// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

const char name_01[] = "filebuf_members-1.tst";
const char name_02[] = "filebuf_members-1.txt";

// Test member functions.
void test_01() 
{
  const char* name_03 = "filebuf_members-3"; // empty file, need to create

  std::filebuf fb_01; // in 
  std::filebuf fb_02; // out
  std::filebuf fb_03; // in | out

  // bool is_open()
  VERIFY( !fb_01.is_open() );
  VERIFY( !fb_02.is_open() );
  VERIFY( !fb_03.is_open() );

  // filebuf_type* open(const char* __s, ios_base::openmode __mode)
  fb_01.open(name_01, std::ios_base::in | std::ios_base::ate);
  fb_02.open(name_02, std::ios_base::in | std::ios_base::out 
	     | std::ios_base::trunc);
  fb_03.open(name_03, std::ios_base::out | std::ios_base::trunc);
  VERIFY( fb_01.is_open() );
  VERIFY( fb_02.is_open() );
  VERIFY( fb_03.is_open() );

  // filebuf_type* close()
  fb_01.close();
  fb_02.close();
  fb_03.close();
  VERIFY( !fb_01.is_open() );
  VERIFY( !fb_02.is_open() );
  VERIFY( !fb_03.is_open() );
}

int
main()
{
  test_01();
  return 0;
}


