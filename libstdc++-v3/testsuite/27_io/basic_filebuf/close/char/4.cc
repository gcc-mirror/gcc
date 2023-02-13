// 2003-05-19  Paolo Carlini  <pcarlini@unitus.it>

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

// { dg-require-fileio "" }

// 27.8.1.3 filebuf member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// Test that upon filebuf::close() 27.8.1.1,3 is enforced.

#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it
const char name_02[] = "filebuf_virtuals-2.txt"; // empty file, need to create

void test_04()
{
  typedef std::filebuf::traits_type 	traits_type;

  std::filebuf fb_01, fb_02;
  char buffer[] = "xxxxxxxxxx";

  // 'in'
  fb_01.open(name_01, std::ios_base::in);
  VERIFY( fb_01.sgetc() != traits_type::eof() );
  VERIFY( fb_01.sbumpc() != traits_type::eof() );
  VERIFY( fb_01.snextc() != traits_type::eof() );
  VERIFY( fb_01.sgetn(buffer, sizeof(buffer)) == sizeof(buffer) );

  fb_01.close();

  VERIFY( fb_01.sgetc() == traits_type::eof() );
  VERIFY( fb_01.sbumpc() == traits_type::eof() );
  VERIFY( fb_01.snextc() == traits_type::eof() );
  VERIFY( fb_01.sgetn(buffer, sizeof(buffer)) == 0 );

  // 'out'
  fb_02.open(name_02, std::ios_base::out);
  VERIFY( fb_02.sputc('T') != traits_type::eof() );
  VERIFY( fb_02.sputn(buffer, sizeof(buffer)) == sizeof(buffer) );

  fb_02.close();
  
  VERIFY( fb_02.sputc('T') == traits_type::eof() );
  VERIFY( fb_02.sputn(buffer, sizeof(buffer)) == 0 );
}

int
main()
{
  test_04();
  return 0;
}
