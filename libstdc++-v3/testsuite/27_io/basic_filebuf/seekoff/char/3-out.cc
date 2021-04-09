// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

void test02(std::filebuf& in, bool pass)
{
  using namespace std;
  typedef streambuf::pos_type pos_type;
  typedef streambuf::off_type off_type;
  pos_type bad = pos_type(off_type(-1));
  pos_type p = 0;

  // seekoff
  p = in.pubseekoff(0, ios_base::beg, ios_base::in);
  VERIFY( pass == (p != bad) );  // See libstdc++/12232

  p = in.pubseekoff(0, ios_base::beg, ios_base::out); 
  VERIFY( pass == (p != bad) );

  p = in.pubseekoff(0, ios_base::beg); 
  VERIFY( pass == (p != bad) );
}

const char name_01[] = "filebuf_virtuals-1.tst"; // file with data in it
const char name_03[] = "filebuf_members-1.tst"; // empty file

int main() 
{
  using namespace std;

  filebuf out1;
  out1.open(name_01, ios_base::out);
  filebuf out2;
  filebuf out3;
  out3.open(name_03, ios_base::out);
  test02(out1, true);
  test02(out2, false);
  test02(out3, true);
  return 0;
}
