// Copyright (C) 2001 Free Software Foundation, Inc.
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

#include <iostream>
#include <fstream>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <testsuite_hooks.h>

// verify that std::filebuf doesn't close files that it didn't open
// when using the following std::filebuf ctor:
//
//      std::filebuf(__c_file_type*  __f,
//                   ios_base::openmode __mode,
//                   int_type  __s);
//
// thanks to "George T. Talbot" <george@moberg.com> for uncovering
// this bug/situation. 

const char name_01[] = "filebuf_members-1.tst";
const char name_02[] = "filebuf_members-1.txt";

int
test_01()
{
  bool test = true;
  int close_num;

  // read (ext)
  FILE* f2 = fopen(name_01, "r");
  VERIFY( f2 != NULL );
  {
    std::filebuf fb(f2, std::ios_base::in, 512);
  }
  close_num = fclose(f2);
  VERIFY( close_num == 0 );


  // read (standard)
  FILE* f = fopen(name_01, "r");
  VERIFY( f != NULL );
  {
    std::ifstream ifstream1(name_01);
    VERIFY( ifstream1.is_open() );
    std::ios_base::iostate st01 = ifstream1.rdstate();
    VERIFY( st01 == std::ios_base::goodbit );
  }
  close_num = fclose(f);
  VERIFY( close_num == 0 );

  
#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

int
test_02()
{
  bool test = true;
  int first_fd = ::open(name_01, O_RDONLY);
  VERIFY( first_fd != -1 );
  FILE* first_file = ::fdopen(first_fd, "r");
  VERIFY( first_file != NULL );
  std::filebuf fb (first_file, std::ios_base::in);

  int second_fd = fb.fd();

  test = first_fd == second_fd;

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

// libstdc++/2913, libstdc++/4879
// John Fardo  <jfardo@laurelnetworks.com>, Brad Garcia <garsh@attbi.com>
void
test_03()
{
  signal(SIGPIPE, SIG_IGN);
  
  if (0 != mkfifo("xxx", S_IRWXU))
    {
      std::cerr << "failed to creat fifo" << std::endl;
      exit(-1);
    }
  
  int fval = fork();
  if (fval == -1)
    {
      std::cerr << "failed to fork" << std::endl;
      unlink("xxx");
      exit(-1);
    }
  else if (fval == 0)
    {
      std::ifstream ifs("xxx");
      sleep(1);
      ifs.close();
      exit(0);
    }

  std::ofstream ofs("xxx");
  sleep(2);
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
      std::cerr << "fail bit was not set!" << std::endl;
      unlink("xxx");
      exit(-1);
    }

  unlink("xxx");
  exit(0);
}

int
main()
{
  test_01();
  test_02();

  test_03();
  return 0;
}
