// 1999-08-16 bkoz
// 1999-11-01 bkoz

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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

// C++98 27.6.2.5.3 basic_ostream inserters

// { dg-final { file-io-diff "ostream_inserter_other_in" "ostream_inserter_other_out" } }

#include <ostream>
#include <fstream>
#include <testsuite_hooks.h>

// via Brent Verner <brent@rcfile.org>
// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00005.html
void
test03(void)
{
  using namespace std;

  typedef ios::pos_type 	pos_type;

  const char* TEST_IN = "ostream_inserter_other_in";
  const char* TEST_OUT = "ostream_inserter_other_out";
  pos_type i_read, i_wrote, rs, ws;
  double tf_size = BUFSIZ * 2.5;
  ofstream testfile(TEST_IN);

  for (int i = 0; i < tf_size; ++i)
    testfile.put('.');
  testfile.close();

  ifstream in(TEST_IN);
  ofstream out(TEST_OUT);
  out << in.rdbuf();
  in.seekg(0, ios_base::beg);
  out.seekp(0, ios_base::beg);
  rs = in.tellg();
  ws = out.tellp();
  in.seekg(0, ios_base::end);
  out.seekp(0, ios_base::end);
  i_read = in.tellg() - rs;
  i_wrote = out.tellp() - ws;
  VERIFY( i_read == i_wrote );
  in.close();
  out.close();
}

int 
main()
{
  test03();
  return 0;
}
