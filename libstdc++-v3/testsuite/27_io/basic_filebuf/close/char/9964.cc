// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 27.8.1.3 filebuf member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// various tests for filebuf::open() and filebuf::close() including
// the non-portable functionality in the libstdc++-v3 IO library

#include <fstream>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <testsuite_hooks.h>

// libstdc++/9964
void test_07()
{
  using namespace std;
  using namespace __gnu_test;
  bool test __attribute__((unused)) = true;

  const char* name = "tmp_fifo3";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  try_mkfifo(name, S_IRWXU);
  
  int child = fork();
  VERIFY( child != -1 );

  if (child == 0)
    {
      filebuf fbin;
      fbin.open(name, ios_base::in);
      sleep(2);
      fbin.close();
      exit(0);
    }
  
  filebuf fb;
  sleep(1);
  filebuf* ret = fb.open(name, ios_base::in | ios_base::out);
  VERIFY( ret != NULL );
  VERIFY( fb.is_open() );

  sleep(3);
  fb.sputc('a');

  ret = fb.close();
  VERIFY( ret != NULL );
  VERIFY( !fb.is_open() );
}

int
main()
{
  test_07();
  return 0;
}
