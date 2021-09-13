// { dg-require-fork "" }
// { dg-require-mkfifo "" }
 
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

// 27.8.1.3 filebuf member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// various tests for filebuf::open() and filebuf::close() including
// the non-portable functionality in the libstdc++-v3 IO library

#include <fstream>
#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <testsuite_hooks.h>

// libstdc++/2913, libstdc++/4879
// John Fardo  <jfardo@laurelnetworks.com>, Brad Garcia <garsh@attbi.com>
bool
test_04()
{
  using namespace __gnu_test;

  bool test = true;
  const char* name = "tmp_fifo1";
  semaphore s1, s2;

  signal(SIGPIPE, SIG_IGN);
  
  unlink(name);
  if (0 != mkfifo(name, S_IRWXU))
    {
      std::cerr << "failed to create fifo" << std::endl;
      exit(-1);
    }
  
  int fval = fork();
  if (fval == -1)
    {
      std::cerr << "failed to fork" << std::endl;
      unlink(name);
      return false;
    }
  else if (fval == 0)
    {
      std::ifstream ifs(name);
      s1.wait();
      ifs.close();
      s2.signal();
      exit(0);
    }

  std::ofstream ofs(name);
  s1.signal();
  s2.wait();
  ofs.put('t');

  /*
   * ISO/IED 14882:1998(E) 27.8.1.10.4
   *
   * void close();
   *
   * Effects:  Calls rdbuf()->close() and, if that function fails
   * (returns a null pointer), calls setstate(failbit)...
   */
  ofs.close();
  if (!(ofs.rdstate() & std::ios::failbit))
    {
      test = false;
    }

  unlink(name);

  return test;
}

int
main()
{
  return !test_04();
}
