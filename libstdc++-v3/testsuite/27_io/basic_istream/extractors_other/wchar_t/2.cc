// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

// 27.6.1.2.3 basic_istream::operator>>
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <fstream>
#include <testsuite_hooks.h>

// filebufs.
void test02() 
{
  const char name_01[] = "wistream_extractor_other-1.txt"; //read 
  const char name_02[] = "wistream_extractor_other-2.txt"; //write

  std::wfilebuf fbin, fbout;
  fbin.open(name_01, std::ios_base::in);
  fbout.open(name_02, std::ios_base::out | std::ios_base::trunc);
  VERIFY( fbin.is_open() );
  VERIFY( fbout.is_open() );

  {
    std::wistream is(&fbin);
    is.unsetf(std::ios_base::skipws);
    is >> &fbout;
  }

  fbout.close();
  fbin.close();
  VERIFY( !fbin.is_open() );
  VERIFY( !fbout.is_open() );
}

int main()
{
  test02();
  return 0;
}
