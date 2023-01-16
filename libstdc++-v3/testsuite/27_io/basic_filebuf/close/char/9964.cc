// { dg-require-fork "" }
// { dg-require-mkfifo "" }

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

// various tests for filebuf::open() and filebuf::close() including
// the non-portable functionality in the libstdc++-v3 IO library

#include <fstream>
#include <cstdlib>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <testsuite_hooks.h>

// libstdc++/9964
bool test_07()
{
  using namespace std;
  using namespace __gnu_test;
  bool test = true;
  semaphore s1, s2;

  const char* name = "tmp_fifo3";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);  
  mkfifo(name, S_IRWXU);
  
  int child = fork();
  test &= bool( child != -1 );

  if (child == 0)
    {
      filebuf fbin;
      fbin.open(name, ios_base::in);
      s1.wait();
      fbin.close();
      s2.signal();
      exit(0);
    }
  
  filebuf fb;
  filebuf* ret = fb.open(name, ios_base::in | ios_base::out);
  test &= bool( ret != 0 );
  test &= bool( fb.is_open() );
  s1.signal();
  s2.wait();
  fb.sputc('a');

  ret = fb.close();
  test &= bool( ret != 0 );
  test &= bool( !fb.is_open() );

  return test;
}

int
main()
{
  return !test_07();
}
