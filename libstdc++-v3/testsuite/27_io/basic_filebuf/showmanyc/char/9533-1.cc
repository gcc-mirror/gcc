// { dg-require-fork "" }
// { dg-require-mkfifo "" }

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

#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fstream>
#include <cstdlib>
#include <testsuite_hooks.h>

// libstdc++/9533
void test_01()
{
  using namespace std;
  using namespace __gnu_test;
  const char* name = "tmp_fifo1";

  const int count = 10000;

  signal(SIGPIPE, SIG_IGN);
  unlink(name);
  
  if (0 != mkfifo(name, S_IRWXU))
    {
      VERIFY( false );
    }
  
  int fval = fork();
  if (fval == -1)
    {
      unlink(name);
      VERIFY( false );
    }
  else if (fval == 0)
    {
      filebuf ofbuf;
      ofbuf.open(name, ios_base::in|ios_base::out);
      VERIFY( ofbuf.is_open() );
      sleep(1);

      for (int i = 0; i < count; ++i)
	ofbuf.sputc(i % 100);

      ofbuf.pubsync();
      sleep(1);
      ofbuf.close();
      exit(0);
    }

  filebuf ifbuf;
  ifbuf.open(name, ios_base::in);
  VERIFY( ifbuf.is_open() );

  for (int j = 0; j < count; ++j)
    {
      filebuf::int_type c1 = ifbuf.sbumpc();
      VERIFY( c1 == j % 100 );
    }

  filebuf::int_type c6 = ifbuf.sbumpc();
  VERIFY( c6 == filebuf::traits_type::eof() );

  sleep(2);
  ifbuf.close();

  unlink(name);
}

int
main() 
{
  test_01();
  return 0;
}

