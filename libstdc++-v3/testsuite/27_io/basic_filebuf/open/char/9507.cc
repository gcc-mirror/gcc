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

// libstdc++/9507
void test_06()
{
  using namespace __gnu_test;
  bool test __attribute__((unused)) = true;
  const char* name = "tmp_fifo2";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);
  try_mkfifo(name, S_IRWXU);
	
  if (!fork())
    {
      std::filebuf fbuf;
      fbuf.open(name, std::ios_base::in);
      fbuf.sgetc();
      sleep(2);
      fbuf.close();
      exit(0);
    }

  std::filebuf fbuf;
  sleep(1);
  std::filebuf* r = fbuf.open(name,
			      std::ios_base::in 
			      | std::ios_base::out
			      | std::ios_base::ate);
  VERIFY( !fbuf.is_open() );
  VERIFY( r == NULL );
}

int
main()
{
  test_06();
  return 0;
}


