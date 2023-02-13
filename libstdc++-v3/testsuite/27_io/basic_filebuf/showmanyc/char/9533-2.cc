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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

// libstdc++/9533
void test_02()
{
  using namespace std;
  const char* name = "tmp_file1";
  const char* strlit = "0123456789";
  
  filebuf fb;

  int written = 0;
  fb.open(name, ios_base::out | ios_base::trunc);	
  for (int i = 0; i < BUFSIZ; ++i)
    written += fb.sputn(strlit, 10);
  fb.close();
  
  int read = 0;
  int n = 0;
  char buf[10];
  fb.open(name, ios_base::in);
  do
    {
      n = fb.sgetn(buf, sizeof(buf));
      read += n;
    }
  while (n);

  VERIFY( read == written );
}

int
main()
{
  test_02();
  return 0;
}
