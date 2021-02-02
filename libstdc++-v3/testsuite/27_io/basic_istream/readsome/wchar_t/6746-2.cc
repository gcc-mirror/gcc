// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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
// causes "in_avail" to return an incorrect value.
// { dg-do run { xfail arm*-*-elf arm*-*-eabi } }

// { dg-require-binary-io "" }
// { dg-require-fileio "" }

// 27.6.1.3 unformatted input functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <fstream>
#include <testsuite_hooks.h>

// libstdc++/6746   
void test13()
{
  using namespace std;
  streamsize sum = 0;
  wifstream ifs("wistream_unformatted-1.tst");
      
  // test01
  size_t i = ifs.rdbuf()->in_avail();
  VERIFY( i != 0 );
    
  // test02
  streamsize extracted;
  do
    {
      wchar_t buf[1024];
      extracted = ifs.readsome(buf, sizeof(buf) / sizeof(wchar_t));
      sum += extracted;
    }
  while (ifs.good() && extracted);
  VERIFY( sum != 0 );  
}
 
int 
main()
{
  test13();
  return 0;
}
