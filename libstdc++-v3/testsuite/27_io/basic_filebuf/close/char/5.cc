// 2003-05-20  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// Test that upon filebuf::close() 27.8.1.1,3 is enforced.

#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it
const char name_02[] = "filebuf_virtuals-2.txt"; // empty file, need to create

bool overflow_called;
bool underflow_called;
bool uflow_called;

class Close_filebuf : public std::filebuf
{
public:
  int_type overflow(int_type c)
  {
    overflow_called = true;
    return std::filebuf::overflow(c);
  }

  int_type underflow()
  {
    underflow_called = true;
    return std::filebuf::underflow();
  }

  int_type uflow()
  {
    uflow_called = true;
    return std::filebuf::uflow();
  }
};

void test_05()
{
  bool test __attribute__((unused)) = true;

  Close_filebuf fb_01, fb_02;
  char buffer[] = "xxxxxxxxxx";

  // 'in'
  fb_01.open(name_01, std::ios_base::in);
  fb_01.sgetc();

  fb_01.close();

  underflow_called = false;
  fb_01.sgetc();
  VERIFY( underflow_called == true );

  uflow_called = false;
  fb_01.sbumpc();
  VERIFY( uflow_called == true );

  uflow_called = false;
  fb_01.snextc();
  VERIFY( uflow_called == true );
 
  uflow_called = false;
  fb_01.sgetn(buffer, sizeof(buffer));
  VERIFY( uflow_called == true );

  // 'out'
  fb_02.open(name_02, std::ios_base::out);

  fb_02.close();

  overflow_called = false;
  fb_02.sputc('T');
  VERIFY( overflow_called == true );

  overflow_called = false;
  fb_02.sputn(buffer, sizeof(buffer));
  VERIFY( overflow_called == true );
}

int
main()
{
  test_05();
  return 0;
}
