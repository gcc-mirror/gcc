// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

// libstdc++/9533
void test_02()
{
  using namespace std;
  bool test = true;  
  const char* name = "tmp_file1";

  const char* strlit = "0123456789";
  
  filebuf fbout;
  fbout.open(name, ios_base::out | ios_base::trunc);
	
  int written = 0;
  for (int i = 0; i < BUFSIZ; ++i)
    written += fbout.sputn(strlit, 10);

  fbout.close();
  
  ifstream in(name);
  int sum = 0;
  bool gotsome;

  do
    {
      char buf[100];
      int n = in.readsome(buf, sizeof(buf));
      gotsome = (n > 0);
      sum += n;
    }
  while (gotsome);

  VERIFY( sum == written );
}

int
main()
{
  test_02();
  return 0;
}
