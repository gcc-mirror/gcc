// { dg-require-mkfifo "" }

// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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
  const char* name = "tmp_fifo2";

  signal(SIGPIPE, SIG_IGN);

  unlink(name);
  mkfifo(name, S_IRWXU);
  
  std::filebuf fbuf;
  // The use of ios_base::ate implies an attempt to seek on the file
  // descriptor.  The seek will fail.  Thus, at the OS level, this
  // call to "fbuf.open" will result in a call to "open" (which will
  // succeed), a call to "lseek" (which will fail), and, finally, a
  // call to "close" (which will succeed).  Thus, after this call, the
  // file should be closed.
  std::filebuf* r = fbuf.open(name,
			      std::ios_base::in 
			      | std::ios_base::out
			      | std::ios_base::ate);
  if (!r)
    VERIFY( !fbuf.is_open() );
  else
    VERIFY( fbuf.is_open() );
}

int
main()
{
  test_06();
  return 0;
}


